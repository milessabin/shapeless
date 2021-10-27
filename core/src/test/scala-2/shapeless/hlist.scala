package shapeless

import org.junit.Assert.assertEquals
import org.junit.Test

trait HListTestsScalaCompat { this: HListTests =>

  type PWS = Product with Serializable with Fruit

  type MIntStringDoubleBound = M[_ >: Double with Int with String]
  type M2IntStringDoubleBound[A] = M2[_ >: Double with Int with String, A]

  type AnyOrMatchable = Any

  @Test
  def testToProductScala2 = {
    import syntax.std.tuple._
    assertEquals(HNil, ().toHList)
  }
}
