package js2s.generator

import js2s.writer.Writer
import org.everit.json.schema.Schema
import org.everit.json.schema.loader.SchemaLoader
import org.json.{JSONObject, JSONTokener}
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.meta.Type

class RootStrategyIntegrationTest extends munit.FunSuite {
  test("data product test case") {
    val res = scala.util.Try{
      val loader = SchemaLoader.builder.schemaJson(new JSONObject(new JSONTokener(getClass.getClassLoader.getResourceAsStream("dataproduct.schema.json")))).draftV6Support.build()
      val parsedSchema = loader.load().build().asInstanceOf[Schema]
      val cn = new NameStrategy()
      val es = new EnumStrategy(cn)
      val ps = new PrimitiveStrategy()
      val cs = new ConstantStrategy(cn)
      val os = new RootStrategy(es, ps, cs, cn)
      val res = os.generate(None, parsedSchema, Map.empty)
      val (nd, allD: Map[Type, SimplifiedDef]) = res.get
      val code = new Writer().materializeFileContents(nd :: allD.values.toList).values.flatten.mkString("\n")
      val toolbox = currentMirror.mkToolBox()
      toolbox.compile(toolbox.parse(code))()
    }
    assertEquals(res.isSuccess, true)
  }
}
