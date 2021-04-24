package shapeless

import org.junit.Assert._
import org.junit.Test
import shapeless.ops.record.Selector

class LabelledGenericTests213 {
  import LabelledGenericTests213._

  @Test
  def testSelectorWithHkt(): Unit = {
    assertEquals(0, new Func[Hkt[Option]].select(Symbol("foo")))
    assertEquals(0, new Func[Hkt[Id]].select(Symbol("foo")))
  }
}

object LabelledGenericTests213 {
  case class Hkt[F[_]](foo: F[String])
  class Func[T] {
    def select[R <: HList](k: Witness)(implicit gen: LabelledGeneric.Aux[T, R], selector: Selector[R, k.T]) = 0
  }
}
