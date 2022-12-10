import js2s.generator._
import js2s.writer.Writer
import org.everit.json.schema.Schema
import org.everit.json.schema.loader.SchemaLoader
import org.json.{JSONObject, JSONTokener}

import java.nio.file.Paths

object WriterApp extends App {
  val w = new Writer()
  val loader = SchemaLoader.builder
    .schemaJson(new JSONObject(new JSONTokener(getClass.getClassLoader.getResourceAsStream("dataproduct.schema.json"))))
    .draftV6Support
    .build()
  val parsedSchema = loader.load().build().asInstanceOf[Schema]
  val cn           = new NameStrategy()
  val es           = new EnumStrategy(cn)
  val ps           = new PrimitiveStrategy()
  val cs           = new ConstantStrategy(cn)
  val os           = new RootStrategy(es, ps, cs, cn)
  val res          = os.generate(None, parsedSchema, Map.empty)
  val (nd, allD)   = res.get
  w.write(nd :: allD.values.toList, "js2s.dummy", Paths.get("src/test", "scala"))
}
