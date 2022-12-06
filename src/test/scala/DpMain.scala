import js2s.generator._
import org.everit.json.schema.Schema
import org.everit.json.schema.loader.SchemaLoader
import org.json.{JSONObject, JSONTokener}

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}

object DpMain extends App {

  val loader = SchemaLoader.builder.schemaJson(new JSONObject(new JSONTokener(getClass.getClassLoader.getResourceAsStream("dataproduct.schema.json")))).draftV6Support.build()
  val parsedSchema = loader.load().build().asInstanceOf[Schema]
  val cn = new NameStrategy()
  val es = new EnumStrategy(cn)
  val ps = new PrimitiveStrategy()
  val cs = new ConstantStrategy(cn)
  val os = new RootStrategy(es, ps, cs, cn)
  val res = os.generate(None, parsedSchema, Map.empty)
  val (nd, allD) = res.get
  private val path: Path = Paths.get("src/test/scala/dummy")
  scala.util.Try(Files.createDirectory(path))
  (show(nd) ++ allD.values.flatMap(show)).foreach {
    case (fn, defn) =>
      val pw = new PrintWriter(s"src/test/scala/dummy/$fn")
      defn.map(_.syntax).foreach(pw.println)
      pw.flush()
      pw.close()
  }

  // this is probably a typeclass somewhere
  def show(nd: SimplifiedDef): Map[String, List[meta.Stat]] = {
    nd match {
      case PrimitiveDef(_) =>
        Map.empty
      case ProductDef(value) =>
        Map(s"${value.name.value}.scala" -> (value :: Nil))
      case EnumDef(root, companion) =>
        Map(
          s"${root.name.value}.scala" -> (root :: companion :: Nil)
        )
      case UnionDef(root, values) =>
        Map(s"${root.name.value}.scala" -> (root :: values))
      case ConstDef(value) =>
        Map(s"${value.name.value}.scala" -> (value :: Nil))
      case ArrayDef(_) =>
        Map.empty
      case MapDef(_) =>
        Map.empty
    }
  }
}