package shapeless

import org.junit.Assert._
import org.junit.Test

import scala.reflect.runtime.universe.WeakTypeTag

class WitnessJvmTests {

  @Test
  def testRuntimeReflection: Unit = {
    val w1: Witness.Lt[Int] = Witness(1)
    val v1 = w1.value
    def infer[T](v: T)(implicit ev: WeakTypeTag[T]): WeakTypeTag[T] = ev
    val tag = infer(v1)
    assertEquals(tag.tpe.toString, "w1.T")
  }
}
