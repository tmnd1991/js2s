package js2s.generator

import org.everit.json.schema._

import scala.jdk.CollectionConverters.collectionAsScalaIterableConverter

class ConstantStrategy(nameChooser: NameStrategy) {

  def generate(fieldName: Option[String], s: Schema): Option[ConstDef] = {
    s match {
      case cs: CombinedSchema =>
        cs.getCriterion.toString match {
          case "allOf" =>
            for {
              n <- nameChooser(fieldName, s)
              constS <- cs.getSubschemas.asScala.collectFirst { case e: ConstSchema => e }
              _ <- cs.getSubschemas.asScala.collectFirst { case s: StringSchema => s }
            } yield {
              ConstDef(ScalaMetaUtils.constDef(n, constS.getPermittedValue.toString))
            }
          case _ => None
        }
      case rs: ReferenceSchema =>
        generate(fieldName, rs.getReferredSchema)
      case _ => None
    }
  }
}
