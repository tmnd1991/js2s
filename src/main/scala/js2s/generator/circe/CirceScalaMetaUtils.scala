package js2s.generator.circe
import js2s.generator.ScalaMetaUtils

import scala.meta._

object CirceScalaMetaUtils {

  val circeImports = q"""import io.circe.Decoder.Result
                         import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}"""

  def buildCodecForEnum(fqn: String): List[Stat] = {
    val name                    = fqn.split("\\.", -1).last
    val modelImporter: Importer = ScalaMetaUtils.buildImport(fqn)

    List(
      Import(List(modelImporter)),
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
    val name                    = fqn.split("\\.", -1).last
    val modelImporter: Importer = ScalaMetaUtils.buildImport(fqn)

    List(
      Import(List(modelImporter)),
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
}
