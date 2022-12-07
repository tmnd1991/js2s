package js2s.writer

import js2s.generator._

import java.io.PrintWriter
import java.nio.file.{Files, Path}

class Writer {

  /**
   *
   * @param pkg  java package like `js2s.dummy`
   * @param path something like src/main/scala
   */
  def write(defs: List[SimplifiedDef], pkg: String, path: Path): Unit = {
    val packagePath = pkg.split("\\.")
    val writePath = packagePath.foldLeft(path)(_.resolve(_))
    scala.util.Try(Files.createDirectory(writePath))
    val filesToWrite = materializeFileContents(defs)
    filesToWrite.foreach {
      case (fn, defn) =>
        val pw = new PrintWriter(s"$writePath/$fn")
        try {
          pw.println(s"package $pkg")
          defn.map(_.syntax).foreach(pw.println)
          pw.flush()
        } finally {
          pw.close()
        }
    }
  }

  def materializeFileContents(defs: List[SimplifiedDef]): Map[String, List[meta.Stat]] = {
    defs.collect {
      case ProductDef(value) =>
        s"${value.name.value}.scala" -> (value :: Nil)
      case EnumDef(root, companion) =>
        s"${root.name.value}.scala" -> (root :: companion :: Nil)
      case UnionDef(root, values) =>
        s"${root.name.value}.scala" -> (root :: values)
      case ConstDef(value) =>
        s"${value.name.value}.scala" -> (value :: Nil)
//      case ArrayDef(_) =>
//        List.empty
//      case MapDef(_) =>
//        List.empty
//      case PrimitiveDef(_) =>
//        List.empty
    }
  }.toMap
}
