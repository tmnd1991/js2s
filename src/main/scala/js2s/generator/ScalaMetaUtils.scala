package js2s.generator

import scala.meta.{Ctor, Decl, Defn, Importee, Importer, Init, Lit, Mod, Pat, Self, Template, Term, Type}

object ScalaMetaUtils {
  def constDef(name: String, value: String): Defn.Object = {
    Defn.Object(Mod.Case() :: Nil, Term.Name(name),
      Template(Nil, Nil, Self(meta.Name(""), None), Defn.Val(Nil, Pat.Var(ValueTerm) :: Nil, Some(StringType), Lit.String(value)) :: Nil)
    )
  }

  def enumDef(name: String, values: Set[String]): EnumDef = {
    val simpleTrait = traitDef(name)

    val traitWithValue = simpleTrait.copy(
      templ = simpleTrait.templ.copy(stats = Decl.Val(Nil, Pat.Var(ValueTerm) :: Nil, StringType) :: Nil)
    )
    val valuesDef = values.toList.map { s =>
      val name = s.capitalize
      Defn.Object(
        mods = Mod.Case() :: Nil,
        name = Term.Name(name),
        templ = Template(
          early = Nil,
          inits = typeNameToInit(traitWithValue.name) :: Nil,
          self = EmptySelf,
          stats = Defn.Val(Nil,
            Pat.Var(ValueTerm) :: Nil,
            Some(StringType),
            Lit.String(s)
          ) :: Nil
        )
      )
    }
    val valueOfParamName = "s"
    val companion = Defn.Object(
      Nil,
      Term.Name(name),
      Template(
        early = Nil,
        inits = Nil,
        self = EmptySelf,
        stats = Defn.Def(
          mods = Nil,
          name = Term.Name("valueOf"),
          tparams = Nil,
          paramss = (Term.Param(Nil, Term.Name(valueOfParamName), Some(StringType), None) :: Nil) :: Nil,
          decltpe = Some(
            optionalType(Type.Name(name))
          ),
          body = Term.Block(
            Term.Match(
              Term.Name(valueOfParamName),
              valuesDef.map { vd =>
                meta.Case(
                  Term.Select(vd.name, ValueTerm),
                  None,
                  applySome(vd)
                )
              } :+ meta.Case(
                Pat.Wildcard(),
                None,
                NoneTerm
              )
            ) :: Nil
          )
        ) :: valuesDef
      )
    )
    EnumDef(traitWithValue, companion)
  }

  private def optionalType(t: Type) = {
    Type.Apply(Type.Name("Option"), t :: Nil)
  }

  def traitDef(name: String): Defn.Trait = {
    val t = Type.Name(name)
    Defn.Trait(
      Mod.Sealed() :: Nil,
      t,
      Nil,
      Ctor.Primary(Nil, meta.Name(""), Nil),
      Template(
        Nil,
        ProductWithSerializable,
        self = EmptySelf,
        stats = Nil,
        derives = Nil
      )
    )
  }

  def addAncestor(defn: Defn.Class, superClass: Type.Name): Defn.Class = {
    defn.transform {
      case t: Template => t.copy(inits = typeNameToInit(superClass) :: Nil)
    }.asInstanceOf[Defn.Class] // I don't like this cast
  }

  def makeOptional(p: Term.Param): Term.Param = {
    p.copy(
      decltpe = p.decltpe.map { declType =>
        optionalType(declType)
      }
    )
  }

  def unionDef(name: String, members: List[Defn.Class]): UnionDef = {
    val root = traitDef(name)
    UnionDef(root,
      members.map(defn =>
        addAncestor(defn, root.name)
      )
    )
  }

  def productDef(name: String,
                 params: List[Term.Param],
                 superClass: Option[Type.Name]): Defn.Class = {
    val t = Type.Name(name)
    Defn.Class(
      mods = Mod.Case() :: Nil,
      name = t,
      tparams = Nil,
      ctor = primaryCtor(params),
      templ = Template(
        early = Nil,
        inits = superClass.map(typeNameToInit).toList,
        self = EmptySelf,
        stats = Nil,
        derives = Nil)
    )
  }

  def mapDef(valueType: Type): MapDef = {
    MapDef(Type.Apply(Type.Name("Map"), List(StringType, valueType)))
  }

  def buildImport(fqn: String): Importer = {
    val splits = fqn.split("\\.", -1).toList
    val importee = Importee.Name(meta.Name(splits.last))

    def r(toProcess: List[String], z: Term.Select): Term.Select = {
      toProcess match {
        case Nil => z
        case h :: t => Term.Select(r(t, z), Term.Name(h))
      }
    }

    val res = splits.dropRight(1) match {
      case first :: Nil =>
        Term.Name(first)
      case first :: second :: t =>
        r(t.reverse, Term.Select(Term.Name(first), Term.Name(second)))
      case Nil => throw new IllegalArgumentException("Import of only one term is not syntactically correct")
    }
    Importer(res, importee :: Nil)
  }

  private val NoneTerm = Term.Name("None")

  private def applySome(vd: Defn.Object) = {
    Term.Apply(Term.Name("Some"), vd.name :: Nil)
  }

  private val EmptySelf = Self(meta.Name(""), None)

  private def primaryCtor(params: List[Term.Param]) = {
    Ctor.Primary(Nil, meta.Name(""), params :: Nil)
  }

  private val ProductWithSerializable = Init(Type.Name("Product"), meta.Name(""), Nil) ::
    Init(Type.Name("Serializable"), meta.Name(""), Nil) :: Nil

  private def typeNameToInit(tName: Type.Name): Init = Init(tName, meta.Name(""), Nil)

  private val ValueTerm = Term.Name("value")
  private val StringType = Type.Name("String")
}

sealed trait SimplifiedDef extends Product with Serializable {
  val t: meta.Type
  val symbols: Map[meta.Type, SimplifiedDef]
}

case class PrimitiveDef(value: Type.Name) extends SimplifiedDef {
  override val symbols: Map[Type, SimplifiedDef] = Map.empty
  val t: Type = value
}

case class ProductDef(value: Defn.Class) extends SimplifiedDef {
  val symbols: Map[meta.Type, SimplifiedDef] = Map(value.name -> this)
  val t: Type = value.name
}

case class EnumDef(root: Defn.Trait, companion: Defn.Object) extends SimplifiedDef {
  val symbols: Map[meta.Type, SimplifiedDef] = Map(root.name -> this)
  val t: Type = root.name
}

case class UnionDef(root: Defn.Trait, values: List[Defn.Class]) extends SimplifiedDef {
  val symbols: Map[meta.Type, SimplifiedDef] = Map(root.name -> this)
  val t: Type = root.name
}

case class ConstDef(value: Defn.Object) extends SimplifiedDef {
  val symbols: Map[meta.Type, SimplifiedDef] = Map(Type.Name(value.name.value) -> this)
  val t: Type = Type.Singleton(value.name)
}

case class ArrayDef(tn: Type) extends SimplifiedDef {
  val symbols: Map[Type, SimplifiedDef] = Map.empty
  val t: Type = tn
}

case class MapDef(tn: Type) extends SimplifiedDef {
  val symbols: Map[Type, SimplifiedDef] = Map.empty
  override val t: Type = tn
}
