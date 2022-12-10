package js2s.generator

import org.everit.json.schema.{CombinedSchema, ConstSchema, Schema}

import scala.jdk.CollectionConverters.collectionAsScalaIterableConverter

/**
 * how to choose the name if it's a constant, it's the constant permitted value
 * if I have a schema location, then the name is the last part of the schema
 * Location capitalized if there is no schema location and I'm a field of an
 * object, then the name is the field name capitalized if previous strategies
 * fail, I use the title, without spaces and with first letters capitalized
 */
class NameStrategy {

  def apply(fieldName: Option[String], s: Schema): Option[String] =
    constantValue(s).orElse(defaultCase(fieldName, s))

  private def defaultCase(fieldName: Option[String], s: Schema) =
    Option(s.getSchemaLocation).flatMap {
      case "#" => None
      case o   => Some(o)
    }
      .map(_.split("/", -1).last.capitalize)
      .orElse(fieldName.map(_.capitalize))
      .orElse(Option(s.getTitle).map(sanitiseTitle))

  private def constantValue(s: Schema): Option[String] =
    s match {
      case cs: CombinedSchema =>
        val value = cs.getSubschemas.asScala.collectFirst { case c: ConstSchema =>
          c.getPermittedValue.toString
        }
        (cs.getCriterion.toString, value) match {
          case ("allOf", Some(v)) =>
            Some(sanitiseTitle(v))
          case _ =>
            None
        }
      case _ =>
        None
    }

  private def sanitiseTitle(s: String): String =
    s.split("\\s+", -1).map(_.capitalize).mkString("")
}
