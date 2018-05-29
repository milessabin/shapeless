package shapeless

import test._
import testutil.assertTypedEquals

import org.junit.Test

class PerhapsTests {
  class A
  class B
  implicit val b: B = new B

  @Test
  def testResolutionSuccess: Unit = {
    assertTypedEquals[Option[B]](the[Perhaps[B]].value, Some(b))
  }

  @Test
  def testResolutionFailure: Unit = {
    assertTypedEquals[Option[A]](the[Perhaps[A]].value, None)
  }

  class C
  implicit val c: C = null

  /**
    * This test documents a wart with this trick wherein if you have an implicit defined but its value is `null`, its
    * resolution will be considered a failure.
    */
  @Test
  def testResolutionFailureIfImplicitDefinedWithNullValue: Unit = {
    assertTypedEquals[Option[C]](the[Perhaps[C]].value, None)
  }
}
