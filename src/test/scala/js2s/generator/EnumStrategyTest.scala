package js2s.generator

import org.everit.json.schema.{ObjectSchema, ReferenceSchema}
import org.everit.json.schema.loader.SchemaLoader
import org.json.{JSONObject, JSONTokener}

import scala.meta._

class EnumStrategyTest() extends munit.FunSuite {

  test("protocol enum") {
    val schema =
      """
        |{
        |  "$id": "",
        |  "title": "",
        |  "description": "",
        |  "type": "object",
        |  "properties": {
        |    "prot": {
        |      "$ref": "#/$defs/protocol"
        |    }
        |  },
        |  "$defs": {
        |    "protocol": {
        |      "type": "string",
        |      "enum": [
        |          "http",
        |          "https"
        |      ]
        |    }
        |  }
        |}
        |""".stripMargin
    val loader = SchemaLoader.builder.schemaJson(new JSONObject(new JSONTokener(schema))).draftV6Support.build()
    val parsedSchema = loader.load().build().asInstanceOf[ObjectSchema]
    val v = parsedSchema.getPropertySchemas.get("prot").asInstanceOf[ReferenceSchema].getReferredSchema
    val res = new EnumStrategy(new NameStrategy).generate(Some("prot"), v)
    assertEquals(res.get.root.structure, q"""sealed trait Protocol extends Product with Serializable{val value: String}""".structure)
    assertEquals(res.get.companion.structure,
      q"""object Protocol {
            def valueOf(s: String): Option[Protocol] = {
              s match {
                case Http.value =>
                  Some(Http)
                case Https.value =>
                  Some(Https)
                case _ =>
                  None
              }
            }
            case object Http extends Protocol {val value: String = "http"}
            case object Https extends Protocol {val value: String = "https"}
          }""".structure
    )
  }
}
