package js2s.generator.circe
import js2s.generator.{ProductDef, UnionDef}

import scala.meta._

object CirceScalaMetaUtils {

  val circeImports = q"""import io.circe.Decoder.Result
                         import io.circe._"""

  val circeImportsForUnions = q"""
                                 import io.circe.syntax._
                                 import cats.syntax.functor._"""

  val circeSemiAutoImport = q"import io.circe.generic.semiauto._"

  def buildCodecForEnum(fqn: String): List[Stat] = {
    val name = fqn.split("\\.", -1).last
    List(
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(Term.Name(s"encode$name"))),
        Some(Type.Apply(Type.Name("Encoder"), List(Type.Name(name)))),
        Term.NewAnonymous(
          Template(
            Nil,
            List(Init(Type.Apply(Type.Name("Encoder"), List(Type.Name(name))), Name(""), Nil)),
            Self(Name(""), None),
            List(
              Defn.Def(
                List(Mod.Final()),
                Term.Name("apply"),
                Nil,
                List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name(name)), None))),
                Some(Type.Name("Json")),
                Term.Apply(
                  Term.Select(Term.Name("Json"), Term.Name("fromString")),
                  List(Term.Select(Term.Name("a"), Term.Name("value")))
                )
              )
            ),
            Nil
          )
        )
      ),
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(Term.Name(s"decode$name"))),
        Some(Type.Apply(Type.Name("Decoder"), List(Type.Name(name)))),
        Term.NewAnonymous(
          Template(
            Nil,
            List(Init(Type.Apply(Type.Name("Decoder"), List(Type.Name(name))), Name(""), Nil)),
            Self(Name(""), None),
            List(
              Defn.Def(
                List(Mod.Final()),
                Term.Name("apply"),
                Nil,
                List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("HCursor")), None))),
                Some(Type.Apply(Type.Select(Term.Name("Decoder"), Type.Name("Result")), List(Type.Name(name)))),
                Term.Block(
                  List(
                    Term.Apply(
                      Term.Select(
                        Term.ApplyType(
                          Term.Select(Term.Select(Term.Name("c"), Term.Name("value")), Term.Name("as")),
                          List(Type.Name("String"))
                        ),
                        Term.Name("flatMap")
                      ),
                      List(
                        Term.Block(
                          List(
                            Term.Function(
                              List(Term.Param(Nil, Term.Name("s"), None, None)),
                              Term.Apply(
                                Term.Select(
                                  Term.Apply(Term.Select(Term.Name(name), Term.Name("valueOf")), List(Term.Name("s"))),
                                  Term.Name("toRight")
                                ),
                                List(
                                  Term.Apply(
                                    Term.Name("DecodingFailure"),
                                    List(
                                      Term.ApplyInfix(
                                        Term.Name("s"),
                                        Term.Name("+"),
                                        Nil,
                                        List(Lit.String(s" did not match any of the supported values for $name"))
                                      ),
                                      Term.Select(Term.Name("c"), Term.Name("history"))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            Nil
          )
        )
      )
    )
  }

  def buildCodecForConst(fqn: String): List[Stat] = {
    val name = fqn.split("\\.", -1).last
    List(
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(Term.Name(s"${name}Encoder"))),
        Some(Type.Apply(Type.Name("Encoder"), List(Type.Singleton(Term.Name(name))))),
        Term.NewAnonymous(
          Template(
            Nil,
            List(Init(Type.Apply(Type.Name("Encoder"), List(Type.Singleton(Term.Name(name)))), Name(""), Nil)),
            Self(Name(""), None),
            List(
              Defn.Def(
                List(Mod.Override()),
                Term.Name("apply"),
                Nil,
                List(
                  List(
                    Term
                      .Param(Nil, Term.Name("a"), Some(Type.Singleton(Term.Name(name))), None)
                  )
                ),
                Some(Type.Name("Json")),
                Term.Apply(
                  Term.Select(Term.Name("Json"), Term.Name("fromString")),
                  List(Term.Select(Term.Name("a"), Term.Name("value")))
                )
              )
            ),
            Nil
          )
        )
      ),
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(Term.Name(s"${name}Decoder"))),
        Some(Type.Apply(Type.Name("Decoder"), List(Type.Singleton(Term.Name(name))))),
        Term.NewAnonymous(
          Template(
            Nil,
            List(Init(Type.Apply(Type.Name("Decoder"), List(Type.Singleton(Term.Name(name)))), Name(""), Nil)),
            Self(Name(""), None),
            List(
              Defn.Def(
                List(Mod.Override()),
                Term.Name("apply"),
                Nil,
                List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("HCursor")), None))),
                Some(Type.Apply(Type.Name("Result"), List(Type.Singleton(Term.Name(name))))),
                Term.Block(
                  List(
                    Term.Apply(
                      Term.Select(
                        Term.ApplyType(Term.Select(Term.Name("c"), Term.Name("as")), List(Type.Name("String"))),
                        Term.Name("flatMap")
                      ),
                      List(
                        Term.PartialFunction(
                          List(
                            Case(
                              Term.Select(Term.Name(name), Term.Name("value")),
                              None,
                              Term.Apply(Term.Name("Right"), List(Term.Name(name)))
                            ),
                            Case(
                              Pat.Var(Term.Name("s")),
                              None,
                              Term.Apply(
                                Term.Name("Left"),
                                List(
                                  Term.Apply(
                                    Term.Name("DecodingFailure"),
                                    List(
                                      Term.ApplyInfix(
                                        Term.Name("s"),
                                        Term.Name("+"),
                                        Nil,
                                        List(Lit.String(s" did not match any of the supported values for $name"))
                                      ),
                                      Term.Select(Term.Name("c"), Term.Name("history"))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            Nil
          )
        )
      )
    )
  }

  private def decoderForUnionCase(tName: Type, discriminatorField: String, discriminatorValue: String): Stat = {
    val lowercaseTname = tName.syntax.toLowerCase
    Defn.Val(
      List(Mod.Implicit()),
      List(Pat.Var(Term.Name(s"${lowercaseTname}Decoder"))),
      Some(Type.Apply(Type.Name("Decoder"), List(tName))),
      Term.Block(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(Term.Name(s"_${lowercaseTname}Decoder"))),
            Some(Type.Apply(Type.Name("Decoder"), List(tName))),
            Term.ApplyType(Term.Name("deriveDecoder"), List(tName))
          ),
          Term.NewAnonymous(
            Template(
              Nil,
              List(Init(Type.Apply(Type.Name("Decoder"), List(tName)), Name(""), Nil)),
              Self(Name(""), None),
              List(
                Defn.Def(
                  List(Mod.Override()),
                  Term.Name("apply"),
                  Nil,
                  List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("HCursor")), None))),
                  Some(Type.Apply(Type.Name("Result"), List(tName))),
                  Term.Apply(
                    Term.Select(
                      Term.ApplyType(
                        Term.Select(
                          Term.Apply(
                            Term.Select(Term.Name("c"), Term.Name("downField")),
                            List(Lit.String(discriminatorField))
                          ),
                          Term.Name("as")
                        ),
                        List(Type.Name("String"))
                      ),
                      Term.Name("flatMap")
                    ),
                    List(
                      Term.PartialFunction(
                        List(
                          Case(
                            Lit.String(discriminatorValue),
                            None,
                            Term.Apply(Term.Name(s"_${lowercaseTname}Decoder"), List(Term.Name("c")))
                          ),
                          Case(
                            Pat.Var(Term.Name("s")),
                            None,
                            Term.Apply(
                              Term.Name("Left"),
                              List(
                                Term.Apply(
                                  Term.Name("DecodingFailure"),
                                  List(
                                    Term.ApplyInfix(
                                      Lit.String("Unknown discriminator value "),
                                      Term.Name("+"),
                                      Nil,
                                      List(Term.Name("s"))
                                    ),
                                    Term.Select(Term.Name("c"), Term.Name("history"))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              Nil
            )
          )
        )
      )
    )
  }

  private def encoderForUnionCase(tName: Type, discriminatorField: String, discriminatorValue: String): Stat = {
    val lowercaseTname = tName.syntax.toLowerCase
    Defn.Val(
      List(Mod.Implicit()),
      List(Pat.Var(Term.Name(s"${lowercaseTname}Encoder"))),
      Some(Type.Apply(Type.Name("Encoder"), List(tName))),
      Term.Block(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(Term.Name(s"_${lowercaseTname}Encoder"))),
            None,
            Term.ApplyType(Term.Name("deriveEncoder"), List(tName))
          ),
          Term.NewAnonymous(
            Template(
              Nil,
              List(Init(Type.Apply(Type.Name("Encoder"), List(tName)), Name(""), Nil)),
              Self(Name(""), None),
              List(
                Defn.Def(
                  List(Mod.Override()),
                  Term.Name("apply"),
                  Nil,
                  List(List(Term.Param(Nil, Term.Name("a"), Some(tName), None))),
                  Some(Type.Name("Json")),
                  Term.Block(
                    List(
                      Defn.Val(
                        Nil,
                        List(Pat.Var(Term.Name("defaultFields"))),
                        None,
                        Term.Select(
                          Term.Select(
                            Term.Apply(
                              Term.Select(Term.Name(s"_${lowercaseTname}Encoder"), Term.Name("encodeObject")),
                              List(Term.Name("a"))
                            ),
                            Term.Name("toIterable")
                          ),
                          Term.Name("toList")
                        )
                      ),
                      Term.Apply(
                        Term.Select(Term.Name("Json"), Term.Name("obj")),
                        List(
                          Term.Repeated(
                            Term.ApplyInfix(
                              Term.ApplyInfix(
                                Lit.String(discriminatorField),
                                Term.Name("->"),
                                Nil,
                                List(
                                  Term.Apply(
                                    Term.Select(Term.Name("Json"), Term.Name("fromString")),
                                    List(Lit.String(discriminatorValue))
                                  )
                                )
                              ),
                              Term.Name("::"),
                              Nil,
                              List(Term.Name("defaultFields"))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              Nil
            )
          )
        )
      )
    )
  }

  private def rootDecoder(unionDef: UnionDef): Stat = {
    val lrootName = unionDef.t.syntax.toLowerCase
    val qs = unionDef.values.map { pd =>
      pd.t.syntax.toLowerCase
    }
    Defn.Val(
      List(Mod.Implicit()),
      List(Pat.Var(Term.Name(s"${lrootName}Decoder"))),
      Some(Type.Apply(Type.Name("Decoder"), List(unionDef.t))),
      Term.Apply(
        Term.Select(
          Term.Apply(
            Term.Name("List"),
            qs.map { lc =>
              Term.ApplyType(Term.Select(Term.Name(s"${lc}Decoder"), Term.Name("widen")), List(unionDef.t))
            }
          ),
          Term.Name("reduceLeft")
        ),
        List(
          Term.AnonymousFunction(Term.ApplyInfix(Term.Placeholder(), Term.Name("or"), Nil, List(Term.Placeholder())))
        )
      )
    )
  }

  private def rootEncoder(unionDef: UnionDef): Stat = {
    val lrootName = unionDef.t.syntax.toLowerCase
    val qs        = unionDef.values.map(_.t)
    Defn.Val(
      List(Mod.Implicit()),
      List(Pat.Var(Term.Name(s"${lrootName}Encoder"))),
      Some(Type.Apply(Type.Name("Encoder"), List(unionDef.t))),
      Term.Apply(
        Term.Select(Term.Name("Encoder"), Term.Name("instance")),
        List(
          Term.PartialFunction(
            qs.map { t =>
              Case(
                Pat.Typed(Pat.Var(Term.Name("v")), t),
                None,
                Term.Select(Term.Name("v"), Term.Name("asJson"))
              )
            }
          )
        )
      )
    )
  }

  def buildCodecForUnion(unionDef: UnionDef): List[Stat] =
    unionDef.values.flatMap { pd =>
      decoderForUnionCase(pd.t, pd.ofUnion.get._1, pd.ofUnion.get._2) ::
        encoderForUnionCase(pd.t, pd.ofUnion.get._1, pd.ofUnion.get._2) :: Nil
    } ++ (rootEncoder(unionDef) :: rootDecoder(unionDef) :: Nil)

  def buildCodecForProduct(productDef: ProductDef): List[Stat] = {
    val lrootName = productDef.t.syntax.toLowerCase
    List(
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(Term.Name(s"${lrootName}Encoder"))),
        Some(Type.Apply(Type.Name("Encoder"), List(productDef.t))),
        Term.ApplyType(Term.Name("deriveEncoder"), List(productDef.t))
      ),
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(Term.Name(s"${lrootName}Decoder"))),
        Some(Type.Apply(Type.Name("Decoder"), List(productDef.t))),
        Term.ApplyType(Term.Name("deriveDecoder"), List(productDef.t))
      )
    )

  }
}
