package js2s.writer.circe

import js2s.generator.circe.CirceScalaMetaUtils
import js2s.generator._

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.meta.Import

class CirceWriter {
  def write(defs: List[SimplifiedDef], pkg: String, path: Path): Unit = {
    val packagePath = pkg.split("\\.")
    val writeFolder = packagePath.foldLeft(path)(_.resolve(_))
    val writePath   = writeFolder.resolve("CirceCodecs.scala")
    scala.util.Try(Files.createDirectory(writeFolder))
    val lines = materializeFileContents(pkg, defs)
    val pw    = new PrintWriter(writePath.toFile)
    try {
      lines.map(_.syntax).foreach(pw.println)
    } finally {
      pw.flush()
      pw.close()
    }
  }

  def materializeFileContents(pkg: String, defs: List[SimplifiedDef]): List[meta.Tree] = {

    val imports = if (isThereAnyUnion(defs)) {
      Import(ScalaMetaUtils.buildImport(pkg + "._") :: Nil) ::
        CirceScalaMetaUtils.circeImports.stats :::
        CirceScalaMetaUtils.circeImportsForUnions.stats
    } else {
      Import(ScalaMetaUtils.buildImport(pkg + "._") :: Nil) ::
        CirceScalaMetaUtils.circeImports.stats
    }

    ScalaMetaUtils.buildPackage(pkg) :: imports ::: defs.collect {
      case EnumDef(root, _) =>
        CirceScalaMetaUtils.buildCodecForEnum(pkg + "." + root.name.value)
      case u @ UnionDef(_, _) =>
        CirceScalaMetaUtils.buildCodecForUnion(u)
      case ConstDef(value, _) =>
        CirceScalaMetaUtils.buildCodecForConst(pkg + "." + value.name.value)
      case _ => //pd@ProductDef(_, _) =>
        Nil // TODO

    }.flatten
  }

  private def isThereAnyUnion(defs: List[SimplifiedDef]): Boolean =
    defs.exists(_.isInstanceOf[UnionDef])

}
