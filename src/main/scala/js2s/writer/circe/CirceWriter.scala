package js2s.writer.circe

import js2s.generator.circe.CirceScalaMetaUtils
import js2s.generator._
import js2s.writer.circe.CirceWriter.{CodecPackage, Pkg}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.meta.Import

object CirceWriter {
  class Pkg(val self: String) extends AnyVal

  class CodecPackage(val self: String) extends AnyVal
}
class CirceWriter {

  def write(defs: List[SimplifiedDef], pkg: String, path: Path): Unit = {
    val codecPackage = pkg + ".codecs"
    val packagePath  = codecPackage.split("\\.")
    val writeFolder  = packagePath.foldLeft(path)(_.resolve(_))
    val writePath    = writeFolder.resolve("CirceCodecs.scala")
    scala.util.Try(Files.createDirectory(writeFolder))
    val lines = materializeFileContents(new CodecPackage(codecPackage), new Pkg(pkg), defs)
    val pw    = new PrintWriter(writePath.toFile)
    try {
      lines.map(_.syntax).foreach(pw.println)
    } finally {
      pw.flush()
      pw.close()
    }
  }

  def materializeFileContents(codecPkg: CodecPackage, modelPackage: Pkg, defs: List[SimplifiedDef]): List[meta.Tree] = {

    val imports = buildImports(modelPackage, defs)

    val stats = defs.collect {
      case EnumDef(root, _) =>
        CirceScalaMetaUtils.buildCodecForEnum(modelPackage.self + "." + root.name.value)
      case u @ UnionDef(_, _) =>
        CirceScalaMetaUtils.buildCodecForUnion(u)
      case ConstDef(value, _) =>
        CirceScalaMetaUtils.buildCodecForConst(modelPackage.self + "." + value.name.value)
      case pd: ProductDef =>
        CirceScalaMetaUtils.buildCodecForProduct(pd)
    }.flatten

    ScalaMetaUtils.buildPackage(codecPkg.self) :: imports ::: ScalaMetaUtils.objectDef("CirceCodecs", stats) :: Nil
  }

  private def buildImports(modelPackage: Pkg, defs: List[SimplifiedDef]) = {
    val modelImport = Import(ScalaMetaUtils.buildImport(modelPackage.self + "._") :: Nil)
    val s =
      if (isThereAnyUnion(defs) && isThereAnyProduct(defs)) {
        CirceScalaMetaUtils.circeSemiAutoImport :: CirceScalaMetaUtils.circeImportsForUnions.stats
      } else if (isThereAnyProduct(defs) && !isThereAnyUnion(defs)) {
        CirceScalaMetaUtils.circeSemiAutoImport :: Nil
      } else {
        Nil
      }
    modelImport :: CirceScalaMetaUtils.circeImports.stats ::: s
  }

  private def isThereAnyUnion(defs: List[SimplifiedDef]): Boolean =
    defs.exists(_.isInstanceOf[UnionDef])

  private def isThereAnyProduct(defs: List[SimplifiedDef]): Boolean =
    defs.exists(_.isInstanceOf[ProductDef])

}
