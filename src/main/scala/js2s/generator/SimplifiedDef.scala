package js2s.generator

import scala.meta.{Defn, Term, Type}

sealed trait SimplifiedDef extends Product with Serializable {
  val t: meta.Type
  val symbols: Map[ComparableType, SimplifiedDef]
}

case class PrimitiveDef(value: Type.Name) extends SimplifiedDef {
  override val symbols: Map[ComparableType, SimplifiedDef] = Map.empty
  val t: Type                                              = value
}

case class RefDef(value: Type.Name) extends SimplifiedDef {
  override val symbols: Map[ComparableType, SimplifiedDef] = Map.empty
  val t: Type                                              = value
}
case class ProductDef(value: Defn.Class, ofUnion: Option[(String, String)]) extends SimplifiedDef {
  val symbols: Map[ComparableType, SimplifiedDef] = Map(ComparableType.of(value.name) -> this)
  val t: Type                                     = value.name

  def withDiscriminator(discriminatorField: String, discriminatorValue: String): ProductDef =
    this.copy(
      value = this.value.copy(
        templ = this.value.templ.copy(
          stats = List(
            Defn.Val(
              Nil,
              List(meta.Pat.Var(Term.Name(discriminatorField))),
              Some(Type.Name("String")),
              meta.Lit.String(discriminatorValue)
            )
          )
        )
      ),
      ofUnion = Some(discriminatorField -> discriminatorValue)
    )
}

case class EnumDef(root: Defn.Trait, companion: Defn.Object) extends SimplifiedDef {
  val symbols: Map[ComparableType, SimplifiedDef] = Map(ComparableType.of(root.name) -> this)
  val t: Type                                     = root.name
}

case class UnionDef(root: Defn.Trait, values: List[ProductDef]) extends SimplifiedDef {
  val symbols: Map[ComparableType, SimplifiedDef] = Map(ComparableType.of(root.name) -> this)
  val t: Type                                     = root.name
}

case class ConstDef(value: Defn.Object, permittedValue: String) extends SimplifiedDef {
  val symbols: Map[ComparableType, SimplifiedDef] = Map(ComparableType.of(Type.Name(value.name.value)) -> this)
  val t: Type                                     = Type.Singleton(value.name)
}

case class ArrayDef(tn: Type) extends SimplifiedDef {
  val symbols: Map[ComparableType, SimplifiedDef] = Map.empty
  val t: Type                                     = tn
}

case class MapDef(tn: Type) extends SimplifiedDef {
  val symbols: Map[ComparableType, SimplifiedDef] = Map.empty
  override val t: Type                            = tn
}

class ComparableType(private val t: Type) {
  override def hashCode(): Int = t.structure.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case null               => false
    case ot: ComparableType => this.t.structure.equals(ot.t.structure)
    case _                  => false
  }
}
object ComparableType {
  def of(t: meta.Type): ComparableType = new ComparableType(t)
}
