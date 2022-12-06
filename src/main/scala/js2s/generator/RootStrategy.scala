package js2s.generator

import org.everit.json.schema._

import scala.jdk.CollectionConverters.{asScalaBufferConverter, collectionAsScalaIterableConverter, mapAsScalaMapConverter}
import scala.meta.{Defn, Term, Type}

class RootStrategy(es: EnumStrategy, ps: PrimitiveStrategy, cs: ConstantStrategy, nameChooser: NameStrategy) {


  def generate(fieldName: Option[String],
               schema: Schema,
               symbols: Map[Type, SimplifiedDef]): Option[(SimplifiedDef, Map[Type, SimplifiedDef])] = {
    schema match {
      case o: ObjectSchema =>
        //
        if (o.getSchemaOfAdditionalProperties != null) {
          val (defn,updatedSymbols) = resolve(None, o.getSchemaOfAdditionalProperties, symbols)
          Some(ScalaMetaUtils.mapDef(defn.t) -> (symbols ++ updatedSymbols))
        } else {
          val requiredProps = o.getRequiredProperties.asScala.toSet
          val (params, updatedSymbols) = o.getPropertySchemas.asScala.toList.foldLeft((List.empty[Term.Param], symbols)) { case ((zParams, sym), (fieldName, s)) =>
            val (newType, newDefs) = this.resolve(Some(fieldName), s, sym)
            val tName = newType.t
            val param = Term.Param(Nil, Term.Name(fieldName), Some(tName), None)
            val realParam = if (requiredProps.contains(fieldName)) param else ScalaMetaUtils.makeOptional(param)
            (realParam :: zParams) -> (sym ++ newDefs)
          }
          nameChooser(fieldName, schema).map { n =>
            ProductDef(ScalaMetaUtils.productDef(n, params, None)) -> updatedSymbols
          }
        }
      case cs: CombinedSchema =>
        cs.getCriterion.toString match {
          case "oneOf" =>
            val (subTypes, updatedSymbols) = cs.getSubschemas.asScala.toList.foldLeft((List.empty[Defn.Class], symbols)) { case ((zSubtypes, sym), s) =>
              val (newType, newDefs) = this.resolve(fieldName, s, sym)
              val nts = newType match {
                case PrimitiveDef(_) => List.empty
                case ProductDef(value) => value :: Nil
                case EnumDef(_, _) => Nil
                case UnionDef(_, _) => Nil
                case ConstDef(_) => Nil
                case ArrayDef(_) => Nil
                case MapDef(_) => Nil
              }
              (nts ::: zSubtypes) -> (sym ++ newDefs)
            }
            nameChooser(fieldName, schema).map(ScalaMetaUtils.unionDef(_, subTypes) -> updatedSymbols)

          case _ => None
        }
      case rs: ReferenceSchema =>
        generate(fieldName, rs.getReferredSchema, symbols)
      case as: ArraySchema =>
        val (it, newSym) = this.resolve(fieldName, as.getAllItemSchema, symbols)
        Some(ArrayDef(Type.Apply(Type.Name("List"), it.t :: Nil)) -> (symbols ++ newSym))
      case _ => None
    }
  }

  private def resolve(fieldName: Option[String],
                      schema: Schema,
                      symbols: Map[Type, SimplifiedDef]): (SimplifiedDef, Map[Type, SimplifiedDef]) = {

    // this is a bit cringe
    ps.generate(schema).map(_ -> symbols)
      .orElse(es.generate(fieldName, schema).map(e => e -> (symbols ++ e.symbols)))
      .orElse(cs.generate(fieldName, schema).map(c => c -> (symbols ++ c.symbols)))
      .orElse(generate(fieldName, schema, symbols).map { case (o, s) => o -> (symbols ++ s ++ Map(o.t -> o)) })
      .getOrElse(throw new RuntimeException(s"Unresolvable schema: ${schema}"))
  }
}