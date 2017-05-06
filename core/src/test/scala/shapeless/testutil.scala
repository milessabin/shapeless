package shapeless

import org.junit.Assert._

object testutil {

  /**
    * WARNING: type parameter 'A' is calculated as least upper bound of 'expected' and 'actual'. This method should not be used.
    * The following snippet will not cause a compilation problem: assertTypedEquals(Record(foo = 1), Record(bar = 1))
    */
  def assertTypedEquals[A](expected: A, actual: A): Unit = assertEquals(expected, actual)

  def assertTypedEquals[T, T1](r1: T)(r2: T1)(implicit ev: T =:= T1) = assertEquals(r1, r1)

  def assertTypedSame[A <: AnyRef](expected: A, actual: A): Unit = assertSame(expected, actual)
}
