package shapeless

import org.junit.Assert._

object testutil {
  def assertTypedEquals[A](expected: A, actual: A): Unit = assertEquals(expected, actual)

  def assertTypedSame[A](expected: A, actual: A): Unit = assertSame(expected, actual)
}
