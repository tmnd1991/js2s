package js2s.generator

import js2s.generator.ScalaMetaUtils._

import scala.meta._

class ScalaMetaUtilsTest extends munit.FunSuite {
  test("add ancestor") {

    assertEquals(
      addAncestor(
        productDef(
          name = "Pippo",
          params = Term.Param(Nil, Term.Name("a"), Some(Type.Name("String")), None) :: Nil,
          superClass = None
        ),
        superClass = Type.Name("Pluto")
      ).structure,
      productDef(
        name = "Pippo",
        params = Term.Param(Nil, Term.Name("a"), Some(Type.Name("String")), None) :: Nil,
        superClass = Some(Type.Name("Pluto"))
      ).structure
    )
  }

  test("sealed trait") {
    assertEquals(traitDef("Giovanni").structure, q"sealed trait Giovanni extends Product with Serializable".structure)
  }

  test("enum") {
    val expectedEnum =
      q"""
      object MyEnum {
        def valueOf(s: String): Option[MyEnum] = {
          s match {
            case EnumValue1.value => Some(EnumValue1)
            case EnumValue2.value => Some(EnumValue2)
            case EnumValue3.value => Some(EnumValue3)
            case _ => None
          }
        }
        case object EnumValue1 extends MyEnum {
          val value: String = "enumValue1"
        }
        case object EnumValue2 extends MyEnum {
          val value: String = "enumValue2"
        }
        case object EnumValue3 extends MyEnum {
          val value: String = "enumValue3"
        }
      }
      sealed trait MyEnum extends Product with Serializable {
        val value: String
      }
  """.stats
    val actualEnum = enumDef("MyEnum", Set("enumValue1", "enumValue2", "enumValue3"))
    assertEquals(actualEnum.companion.structure, expectedEnum.head.structure)
    assertEquals(actualEnum.root.structure, expectedEnum.drop(1).head.structure)
  }

  test("union") {
    val expectedUnion = q"""sealed trait Person extends Product with Serializable
      case class Customer(name: String) extends Person { val t: String = "customer" }
      case class NotCustomer(name: String) extends Person { val t: String = "notCustomer" }
   """.stats match {
      case (root: Defn.Trait) :: (one: Defn.Class) :: (two: Defn.Class) :: Nil =>
        UnionDef(root, ProductDef(one, Some("t" -> "customer")) :: ProductDef(two, Some("t" -> "notCustomer")) :: Nil)
      case _ =>
        fail("did not generate expected tree")
    }
    val actualUnion = unionDef(
      "Person",
      List(
        ProductDef(
          productDef(
            "Customer",
            Term.Param(Nil, Term.Name("name"), Some(Type.Name("String")), None) :: Nil,
            None
          ),
          None
        ).withDiscriminator("t", "customer"),
        ProductDef(
          productDef(
            "NotCustomer",
            Term.Param(Nil, Term.Name("name"), Some(Type.Name("String")), None) :: Nil,
            None
          ),
          None
        ).withDiscriminator("t", "notCustomer")
      )
    )
    assertEquals(expectedUnion.root.structure, actualUnion.root.structure)
    assertEquals(expectedUnion.values.map(_.value.structure), actualUnion.values.map(_.value.structure))
    assertEquals(expectedUnion.values.map(_.ofUnion), actualUnion.values.map(_.ofUnion))

  }

  test("make optional") {
    assertEquals(
      makeOptional(q"""def f(a: Int): String = a.toString()""".paramss.head.head).structure,
      q"def f(a: Option[Int]): Option[String] = a.map(_.toString)".paramss.head.head.structure
    )
  }

  test("buildImport 5") {
    val expected = q"import a.b.c.d.e.P"
    val actual   = Import(buildImport("a.b.c.d.e.P") :: Nil)
    assertEquals(actual.structure, expected.structure)
  }

  test("buildImport 1") {
    val expected = q"import a.P"
    val actual   = Import(buildImport("a.P") :: Nil)
    assertEquals(actual.structure, expected.structure)
  }

  test("buildImport 2") {
    val expected = q"import a.b.P"
    val actual   = Import(buildImport("a.b.P") :: Nil)
    assertEquals(actual.structure, expected.structure)
  }
}
