package js2s.generator

import org.everit.json.schema._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.{
  asScalaBufferConverter,
  collectionAsScalaIterableConverter,
  mapAsScalaMapConverter
}
import scala.meta.Term

class RootStrategy(es: EnumStrategy, ps: PrimitiveStrategy, cs: ConstantStrategy, nameChooser: NameStrategy) {

  private def checkDiscriminator(discriminatorField: Option[String], as: ArraySchema): Unit =
    if (discriminatorField.isDefined) {
      throw new RuntimeException(s"an array cannot be part of a 'oneOf': $as")
    }

  private def checkDiscriminator(discriminatorField: Option[String], os: ObjectSchema): Unit =
    discriminatorField match {
      case Some(field) =>
        if (!os.getRequiredProperties.contains(field)) {
          throw new RuntimeException(s"discriminator field ($field) should be a required property of the object: $os")
        }
      case None =>
    }

  private def checkDiscriminator(discriminatorField: Option[String], cs: CombinedSchema): Unit =
    if (discriminatorField.isDefined) {
      throw new RuntimeException(s"a combined schema cannot be part of a 'oneOf': $cs")
    }

  @tailrec
  private def generate(
    fieldName: Option[String],
    schema: Schema,
    symbols: Map[ComparableType, SimplifiedDef],
    discriminatorField: Option[String]
  ): Option[(SimplifiedDef, Map[ComparableType, SimplifiedDef])] =
    schema match {
      case o: ObjectSchema =>
        checkDiscriminator(discriminatorField, o)
        mapCase(o, symbols).orElse(productCase(fieldName, symbols, o, discriminatorField))
      case cs: CombinedSchema =>
        checkDiscriminator(discriminatorField, cs)
        cs.getCriterion.toString match {
          case "oneOf" => unionCase(fieldName, schema, symbols, cs)
          case _       => None
        }
      case rs: ReferenceSchema =>
        generate(fieldName, rs.getReferredSchema, symbols, discriminatorField)
      case as: ArraySchema =>
        checkDiscriminator(discriminatorField, as)
        val (it, newSym) = this.resolve(fieldName, as.getAllItemSchema, symbols)
        Some(ScalaMetaUtils.arrayDef(it.t) -> (symbols ++ newSym))
      case _ => None
    }
  def generate(
    fieldName: Option[String],
    schema: Schema,
    symbols: Map[ComparableType, SimplifiedDef]
  ): Option[(SimplifiedDef, Map[ComparableType, SimplifiedDef])] = generate(fieldName, schema, symbols, None)

  private def unionCase(
    fieldName: Option[String],
    schema: Schema,
    symbols: Map[ComparableType, SimplifiedDef],
    cs: CombinedSchema
  ) = {
    val discriminator = Option(cs.getUnprocessedProperties.get("discriminator"))
      .map(_.toString)
      .orElse(throw new RuntimeException(s"unions must have a discriminator field: $cs does not"))

    val (subTypes, updatedSymbols) = cs.getSubschemas.asScala.toList.foldLeft((List.empty[ProductDef], symbols)) {
      case ((zSubtypes, sym), s) =>
        val (newType, newDefs) = this
          .generate(fieldName, s, sym, discriminator)
          .getOrElse(throw new RuntimeException(s"only products can be part of 'oneOf'"))

        val nts = newType match {
          case pd: ProductDef => pd :: Nil
          case _              => Nil
        }
        (nts ::: zSubtypes) -> (sym ++ newDefs)
    }
    nameChooser(fieldName, schema).map(ScalaMetaUtils.unionDef(_, subTypes) -> updatedSymbols)
  }

  private def productCase(
    fieldName: Option[String],
    symbols: Map[ComparableType, SimplifiedDef],
    o: ObjectSchema,
    discriminatorField: Option[String]
  ) = {
    val requiredProps = o.getRequiredProperties.asScala.toSet
    val propsToProcess = discriminatorField.fold(o.getPropertySchemas.asScala.toList) { f =>
      o.getPropertySchemas.asScala.toList.filter(_._1 != f)
    }
    val (params, updatedSymbols) = propsToProcess.foldLeft((List.empty[Term.Param], symbols)) {
      case ((zParams, sym), (fieldName, s)) =>
        val (newType, newDefs) = this.resolve(Some(fieldName), s, sym)
        val tName              = newType.t
        val param              = Term.Param(Nil, Term.Name(fieldName), Some(tName), None)
        val realParam          = if (requiredProps.contains(fieldName)) param else ScalaMetaUtils.makeOptional(param)
        (realParam :: zParams) -> (sym ++ newDefs)
    }
    nameChooser(fieldName, o).map { n =>
      val withoutDisc = ScalaMetaUtils.productDef(n, params, None)
      discriminatorField.fold(ProductDef(withoutDisc, None) -> updatedSymbols) { df =>
        val v = cs.generate(Some(df), o.getPropertySchemas.get(df)).get.permittedValue
        ProductDef(withoutDisc, None).withDiscriminator(df, v) -> updatedSymbols
      }
    }
  }

  private def mapCase(o: ObjectSchema, symbols: Map[ComparableType, SimplifiedDef]) =
    Option(o.getSchemaOfAdditionalProperties).map { schemaOfAdditionalProps =>
      val (defn, updatedSymbols) = resolve(None, schemaOfAdditionalProps, symbols)
      ScalaMetaUtils.mapDef(defn.t) -> (symbols ++ updatedSymbols)
    }

  private def resolve(
    fieldName: Option[String],
    schema: Schema,
    symbols: Map[ComparableType, SimplifiedDef]
  ): (SimplifiedDef, Map[ComparableType, SimplifiedDef]) =
    // this is a bit cringe
    ps.generate(schema)
      .map(_ -> symbols)
      .orElse(es.generate(fieldName, schema).map(e => e -> (symbols ++ e.symbols)))
      .orElse(cs.generate(fieldName, schema).map(c => c -> (symbols ++ c.symbols)))
      .orElse(generate(fieldName, schema, symbols).map { case (o, s) =>
        o -> (symbols ++ s ++ Map(ComparableType.of(o.t) -> o))
      })
      .getOrElse(throw new RuntimeException(s"Unresolvable schema: $schema"))
}
