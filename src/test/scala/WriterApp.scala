import js2s.generator._
import js2s.writer.Writer
import js2s.writer.circe.CirceWriter
import org.everit.json.schema.Schema
import org.everit.json.schema.loader.SchemaLoader
import org.json.{JSONObject, JSONTokener}

import java.nio.file.{Path, Paths}

object WriterApp extends App {
  val w     = new Writer()
  val cw    = new CirceWriter()
  val sPath = "schema_entity_v1.schema.json"
  val loader = SchemaLoader.builder
    .schemaJson(
      new JSONObject(new JSONTokener(getClass.getClassLoader.getResourceAsStream(sPath)))
    )
    .draftV6Support
    .build()
  val parsedSchema       = loader.load().build().asInstanceOf[Schema]
  val cn                 = new NameStrategy()
  val es                 = new EnumStrategy(cn)
  val ps                 = new PrimitiveStrategy()
  val cs                 = new ConstantStrategy(cn)
  val os                 = new RootStrategy(es, ps, cs, cn)
  val res                = os.generate(None, parsedSchema, Set.empty, Map.empty)
  val (nd, wip, allD)    = res.get.ensuring(_._2.isEmpty)
  val defs               = allD.values.toList
  private val pkg        = "js2s.dummy"
  private val path: Path = Paths.get("src/test", "scala")
  w.write(nd :: defs, pkg, path)
  cw.write(nd.asInstanceOf[ProductDef], defs, pkg, path, sPath)
}
