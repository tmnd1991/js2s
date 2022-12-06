package js2s.generator

import org.everit.json.schema.Schema

/**
 *  how to choose the name
 *  if I have a schema location, then the name is the last part of the schema Location capitalized
 *  if there is no schema location and I'm a field of an object, then the name is the field name capitalized
 *  if previous strategies fail, I use the title, without spaces and with first letters capitalized
 *  */
class NameStrategy {

  private def sanitiseTitle(s: String): String = {
    s.split("\\s+", -1).map(_.capitalize).mkString("")
  }

  def apply(fieldName: Option[String], s: Schema) =

    Option(s.getSchemaLocation).flatMap{
      case "#" => None
      case o => Some(o)
    }
    .map(_.split("/", -1).last.capitalize)
    .orElse(fieldName.map(_.capitalize))
    .orElse(Option(s.getTitle).map(sanitiseTitle))
}
