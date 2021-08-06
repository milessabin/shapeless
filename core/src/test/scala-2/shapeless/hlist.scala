package shapeless

import org.junit.Assert.assertEquals
import org.junit.Test

trait HListTestsScalaCompat { this: HListTests =>

  type PWS = Product with Serializable with Fruit

  type IntStringDoubleBound >: Double with Int with String
  type AnyOrMatchable = Any

  @Test
  def testToProductScala2 = {
    import syntax.std.tuple._
    assertEquals(HNil, ().toHList)
  }
}
