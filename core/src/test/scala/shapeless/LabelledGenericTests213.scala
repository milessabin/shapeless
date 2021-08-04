package shapeless

import org.junit.Assert._
import org.junit.Test
import shapeless.ops.record.Selector

class LabelledGenericTests213 {
  import LabelledGenericTests213._

  @Test
  def testSelectorWithHkt(): Unit = {
    assertEquals(0, new Func[Hkt[Option]].select("foo"))
    assertEquals(0, new Func[Hkt[Id]].select("foo"))
  }
}

object LabelledGenericTests213 {
  case class Hkt[F[_]](foo: F[String])
  class Func[T] {
    def select[R <: HList, K <: Singleton](k: K)(implicit gen: LabelledGeneric.Aux[T, R], selector: Selector[R, K]) = 0
  }
}
