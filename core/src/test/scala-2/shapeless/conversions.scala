package shapeless

import org.junit.Test
import org.junit.Assert._

class ConversionsTestsScala2 {
  import ops.function.{ FnToProduct, FnFromProduct }
  import syntax.std.function._
  import syntax.std.tuple._
  import test._

  @Test
  def testFunctions: Unit = {
    class HListSyntax[A <: HList, F <: AnyRef](a: A) {
      def applied[U](f: F)(implicit cftp: FnToProduct.Aux[f.type, A => U]): U = cftp(f)(a)
    }

    implicit def mkSyntax[A <: HList, F <: AnyRef](a: A)
                                                  (implicit ffp: FnFromProduct.Aux[A => Any, F]): HListSyntax[A, F] =
      new HListSyntax[A, F](a)

    val res = (2 :: "a" :: 1.3 :: HNil) applied ((i, s, d) => (s * i, d * i)) // Function argument types inferred

    assert((res: (String, Double)) == ("aa", 2.6))
  }
}
