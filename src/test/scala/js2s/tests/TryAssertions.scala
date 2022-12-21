package js2s.tests

import munit._

import scala.util.Try
trait TryAssertions extends Assertions {
  def assertIsSuccess[A](res: Try[A])(implicit loc: Location): Unit =
    assertEquals(res.isSuccess, true, res.get)
}
