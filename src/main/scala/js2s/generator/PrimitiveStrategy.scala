package js2s.generator

import org.everit.json.schema._

import scala.meta.Type

class PrimitiveStrategy {
  def generate(schema: Schema): Option[PrimitiveDef] = {
    schema match {
      case _: StringSchema => Some(PrimitiveDef(Type.Name("String")))
      case ns: NumberSchema =>
        if (ns.requiresInteger()) {
          Option(ns.getUnprocessedProperties.get("size")).map(_.toString) match {
            case Some("64") => Some(PrimitiveDef(Type.Name("Long")))
            case Some("32") | None => Some(PrimitiveDef(Type.Name("Int")))
            case Some(_) => None
          }
        } else {
          Option(ns.getUnprocessedProperties.get("format")).map(_.toString) match {
            case Some("32") | None => Some(PrimitiveDef(Type.Name("Float")))
            case Some("64") => Some(PrimitiveDef(Type.Name("Double")))
            case Some(_) => None
          }
        }
      case _: BooleanSchema => Some(PrimitiveDef(Type.Name("Boolean")))
      case rs: ReferenceSchema => generate(rs.getReferredSchema)
      case _ => None
    }
  }

}
