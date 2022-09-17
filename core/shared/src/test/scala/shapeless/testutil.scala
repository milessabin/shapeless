package shapeless

import org.junit.Assert._

object testutil {

  /**
    * WARNING: type parameter 'A' is inferred as least upper bound of 'expected' and 'actual'.
    * The following snippet will not cause a compilation problem: assertTypedEquals(Record(foo = 1), Record(bar = 1))
    */
  def assertTypedEquals[A](expected: A, actual: A): Unit = assertEquals(expected, actual)

  def assertTypedEquals[A](expected: A): A => Unit = assertEquals(expected, _)

  def assertTypedSame[A <: AnyRef](expected: A, actual: A): Unit = assertSame(expected, actual)
}
