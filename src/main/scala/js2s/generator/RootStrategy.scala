package js2s.generator

import org.everit.json.schema._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.{
  asScalaBufferConverter,
  collectionAsScalaIterableConverter,
  mapAsScalaMapConverter
}
import cats.syntax.option._
import scala.meta.{Term, Type}

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
    wipSymbols: Set[ComparableType],
    symbols: Map[ComparableType, SimplifiedDef],
    discriminatorField: Option[String]
  ): Option[(SimplifiedDef, Set[ComparableType], Map[ComparableType, SimplifiedDef])] =
    schema match {
      case o: ObjectSchema =>
        checkDiscriminator(discriminatorField, o)
        mapCase(o, wipSymbols, symbols).orElse(productCase(fieldName, wipSymbols, symbols, o, discriminatorField))
      case cs: CombinedSchema =>
        checkDiscriminator(discriminatorField, cs)
        cs.getCriterion.toString match {
          case "oneOf" => unionCase(fieldName, schema, wipSymbols, symbols, cs)
          case _       => None
        }
      case rs: ReferenceSchema =>
        generate(fieldName, rs.getReferredSchema, wipSymbols, symbols, discriminatorField)
      case as: ArraySchema =>
        checkDiscriminator(discriminatorField, as)
        val (it, updatedWipSymbols, newSym) = this.resolve(fieldName, as.getAllItemSchema, wipSymbols, symbols)
        (ScalaMetaUtils.arrayDef(it.t), wipSymbols ++ updatedWipSymbols, symbols ++ newSym).some
      case _ => None
    }

  def generate(
    fieldName: Option[String],
    schema: Schema,
    wipSymbols: Set[ComparableType],
    symbols: Map[ComparableType, SimplifiedDef]
  ): Option[(SimplifiedDef, Set[ComparableType], Map[ComparableType, SimplifiedDef])] =
    generate(fieldName, schema, wipSymbols, symbols, None)

  private def unionCase(
    fieldName: Option[String],
    schema: Schema,
    wipSymbols: Set[ComparableType],
    symbols: Map[ComparableType, SimplifiedDef],
    cs: CombinedSchema
  ) = {
    val discriminator = Option(cs.getUnprocessedProperties.get("discriminator"))
      .map(_.toString)
      .orElse(throw new RuntimeException(s"unions must have a discriminator field: $cs does not"))
    nameChooser(fieldName, schema).flatMap { n =>
      val myTypeName = Type.Name(n)
      val myType     = ComparableType.of(myTypeName)
      if (wipSymbols.contains(myType)) {
        (RefDef(myTypeName), wipSymbols, symbols).some
      } else {
        val (subTypes, _, updatedSymbols) =
          cs.getSubschemas.asScala.toList.foldLeft((List.empty[ProductDef], wipSymbols ++ Set(myType), symbols)) {
            case ((zSubtypes, wip, sym), s) =>
              val (newType, newWip, newDefs) = this
                .generate(fieldName, s, wip, sym, discriminator)
                .getOrElse(throw new RuntimeException(s"only products can be part of 'oneOf'"))

              val nts = newType match {
                case pd: ProductDef => pd :: Nil
                case _              => Nil
              }
              (nts ::: zSubtypes, wip ++ newWip, sym ++ newDefs)
          }
        Some((ScalaMetaUtils.unionDef(n, subTypes), wipSymbols -- Set(myType), updatedSymbols))
      }
    }
  }

  private def productCase(
    fieldName: Option[String],
    wipSymbols: Set[ComparableType],
    symbols: Map[ComparableType, SimplifiedDef],
    o: ObjectSchema,
    discriminatorField: Option[String]
  ) =
    nameChooser(fieldName, o).flatMap { n =>
      val myTypeName = Type.Name(n)
      val myType     = ComparableType.of(myTypeName)
      if (wipSymbols.contains(myType)) {
        (RefDef(myTypeName), wipSymbols, symbols).some
      } else {
        val requiredProps = o.getRequiredProperties.asScala.toSet
        val propsToProcess = discriminatorField.fold(o.getPropertySchemas.asScala.toList) { f =>
          o.getPropertySchemas.asScala.toList.filter(_._1 != f)
        }
        val (params, _, updatedSymbols) =
          propsToProcess.foldLeft((List.empty[Term.Param], wipSymbols, symbols)) {
            case ((zParams, wip, sym), (fieldName, s)) =>
              val (newType, newWips, newDefs) = this.resolve(Some(fieldName), s, wip, sym)
              val tName                       = newType.t
              val param                       = Term.Param(Nil, Term.Name(fieldName), Some(tName), None)
              val realParam                   = if (requiredProps.contains(fieldName)) param else ScalaMetaUtils.makeOptional(param)
              (realParam :: zParams, wip ++ newWips, sym ++ newDefs)
          }
        val withoutDisc = ScalaMetaUtils.productDef(n, params, None)
        discriminatorField
          .fold((ProductDef(withoutDisc, None), wipSymbols -- Set(myType), updatedSymbols)) { df =>
            val v = cs.generate(Some(df), o.getPropertySchemas.get(df)).get.permittedValue
            (ProductDef(withoutDisc, None).withDiscriminator(df, v), wipSymbols -- Set(myType), updatedSymbols)
          }
          .some
      }
    }

  private def mapCase(o: ObjectSchema, wipSymbols: Set[ComparableType], symbols: Map[ComparableType, SimplifiedDef]) =
    Option(o.getSchemaOfAdditionalProperties).map { schemaOfAdditionalProps =>
      val (defn, newWipSymbols, updatedSymbols) = resolve(None, schemaOfAdditionalProps, wipSymbols, symbols)
      (ScalaMetaUtils.mapDef(defn.t), wipSymbols ++ newWipSymbols, symbols ++ updatedSymbols)
    }

  private def resolve(
    fieldName: Option[String],
    schema: Schema,
    wipSymbols: Set[ComparableType],
    symbols: Map[ComparableType, SimplifiedDef]
  ): (SimplifiedDef, Set[ComparableType], Map[ComparableType, SimplifiedDef]) =
    // this is a bit cringe
    ps.generate(schema)
      .map(x => (x, wipSymbols, symbols))
      .orElse(es.generate(fieldName, schema).map(e => (e, wipSymbols, symbols ++ e.symbols)))
      .orElse(cs.generate(fieldName, schema).map(c => (c, wipSymbols, symbols ++ c.symbols)))
      .orElse(generate(fieldName, schema, wipSymbols, symbols).map { case (o, w, s) =>
        (o, wipSymbols ++ w, symbols ++ s ++ Map(ComparableType.of(o.t) -> o))
      })
      .getOrElse(throw new RuntimeException(s"Unresolvable schema: $schema"))
}
