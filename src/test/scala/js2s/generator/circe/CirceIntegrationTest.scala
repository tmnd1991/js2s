package js2s.generator.circe

import js2s.generator.{
  ComparableType,
  ConstantStrategy,
  EnumStrategy,
  NameStrategy,
  PrimitiveStrategy,
  ProductDef,
  RootStrategy,
  SimplifiedDef
}
import js2s.tests.{TryAssertions, tags}
import js2s.writer.Writer
import js2s.writer.circe.CirceWriter
import js2s.writer.circe.CirceWriter.{CodecPackage, Pkg}
import org.everit.json.schema.Schema
import org.everit.json.schema.loader.SchemaLoader
import org.json.{JSONObject, JSONTokener}

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

class CirceIntegrationTest extends munit.FunSuite with TryAssertions {
  test("data product test case".tag(tags.slow)) {
    assertIsSuccess(tryGenAndCompile("dataproduct.schema.json"))
  }

  test("schema_entity_v1 test case".tag(tags.slow)) {
    assertIsSuccess(tryGenAndCompile("schema_entity_v1.schema.json"))
  }

  private def tryGenAndCompile(schemaFile: String) =
    scala.util.Try {
      val loader = SchemaLoader.builder
        .schemaJson(
          new JSONObject(new JSONTokener(getClass.getClassLoader.getResourceAsStream(schemaFile)))
        )
        .draftV6Support
        .build()
      val parsedSchema                                      = loader.load().build().asInstanceOf[Schema]
      val cn                                                = new NameStrategy()
      val es                                                = new EnumStrategy(cn)
      val ps                                                = new PrimitiveStrategy()
      val cs                                                = new ConstantStrategy(cn)
      val os                                                = new RootStrategy(es, ps, cs, cn)
      val res                                               = os.generate(None, parsedSchema, Set.empty, Map.empty)
      val (nd, _, allD: Map[ComparableType, SimplifiedDef]) = res.get
      val code                                              = new Writer().materializeFileContents(nd :: allD.values.toList).values.flatten.mkString("\n")
      val circeCode = new CirceWriter()
        .materializeFileContents(
          new CodecPackage("test"),
          new Pkg("test"),
          nd.asInstanceOf[ProductDef],
          allD.values.toList,
          schemaFile
        )
        .map(_.syntax)
        .drop(1)
        .mkString("\n")
      val toolbox = currentMirror.mkToolBox()
      toolbox.compile(toolbox.parse(code + "\n" + circeCode))()
    }
}
