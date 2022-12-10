package js2s.generator

import org.everit.json.schema._

import scala.jdk.CollectionConverters.collectionAsScalaIterableConverter

class EnumStrategy(nameChooser: NameStrategy) {
  def generate(fieldName: Option[String], s: Schema): Option[EnumDef] =
    s match {
      case cs: CombinedSchema =>
        cs.getCriterion.toString match {
          case "allOf" =>
            for {
              n  <- nameChooser(fieldName, s)
              es <- cs.getSubschemas.asScala.collectFirst { case e: EnumSchema => e }
              _  <- cs.getSubschemas.asScala.collectFirst { case s: StringSchema => s }
            } yield {
              ScalaMetaUtils.enumDef(n, es.getPossibleValues.asScala.map(_.toString).toSet)
            }
          case _ => None
        }
      case rs: ReferenceSchema =>
        generate(fieldName, rs.getReferredSchema)
      case _ => None
    }
}
