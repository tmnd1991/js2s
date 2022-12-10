package js2s.generator.circe

class CirceScalaMetaUtilsTest extends munit.FunSuite {

  import scala.meta._

  test("create encoder for enum") {
    val expected =
      q"""
                        import js2s.dummy.Protocol
                        implicit val encodeProtocol: Encoder[Protocol] = new Encoder[Protocol] {
                          final def apply(a: Protocol): Json = Json.fromString(a.value)
                        }

                        implicit val decodeProtocol: Decoder[Protocol] = new Decoder[Protocol] {
                          final def apply(c: HCursor): Decoder.Result[Protocol] = {
                            c.value.as[String].flatMap { s =>
                              Protocol
                                .valueOf(s)
                                .toRight(DecodingFailure(s + " did not match any of the supported values for Protocol", c.history))
                            }
                          }
                        }
                      """

    assertEquals(
      expected.stats.map(_.structure),
      CirceScalaMetaUtils.buildCodecForEnum("js2s.dummy.Protocol").map(_.structure)
    )
  }

  test("create encoder for const") {
    val expected = q"""
    import js2s.dummy.zio.pippo.SpeccoVersione

    implicit val SpeccoVersioneEncoder: Encoder[SpeccoVersione.type] = new Encoder[SpeccoVersione.type] {
      override def apply(a: SpeccoVersione.type): Json = Json.fromString(a.value)
    }
    implicit val SpeccoVersioneDecoder: Decoder[SpeccoVersione.type] = new Decoder[SpeccoVersione.type] {
      override def apply(c: HCursor): Result[SpeccoVersione.type] = {
        c.as[String].flatMap({
          case SpeccoVersione.value =>
            Right(SpeccoVersione)
          case s =>
            Left(DecodingFailure(s + " did not match any of the supported values for SpeccoVersione", c.history))
        })
      }
    }"""

    assertEquals(
      CirceScalaMetaUtils.buildCodecForConst("js2s.dummy.zio.pippo.SpeccoVersione").map(_.structure),
      expected.stats.map(_.structure)
    )

  }

}
