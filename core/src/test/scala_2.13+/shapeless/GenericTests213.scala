package shapeless

import org.junit.Assert.assertEquals
import org.junit.Test
import shapeless.test.{illTyped, typed}
import shapeless.testutil.assertTypedEquals

object GenericTests213 {
  case class WrongApplySignature private(value: String)
  object WrongApplySignature {
    // We can't replace the synthetic `apply` method on Scala 2.11
    def apply(v: String): Either[String, WrongApplySignature] = Left("No ways")
  }

  case class CCWithCustomUnapply(x: Int, y: String)
  object CCWithCustomUnapply {
    def unapply(cc: CCWithCustomUnapply): Option[(Int, String, String)] = None
  }
}

class GenericTests213 {
  import GenericTests213._

  illTyped("Generic[WrongApplySignature]")

  @Test
  def testCCWithCustomUnapply(): Unit = {
    val cc = CCWithCustomUnapply(23, "foo")
    val gen = Generic[CCWithCustomUnapply]
    val r = gen.to(cc)
    val f = gen.from(13 :: "bar" :: HNil)
    assertTypedEquals[Int :: String :: HNil](23 :: "foo" :: HNil, r)
    typed[CCWithCustomUnapply](f)
    assertEquals(13, f.x)
    assertEquals("bar", f.y)
  }
}
