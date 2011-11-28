import org.junit.Test
import org.junit.Assert._

class ConversionTests {
  import Tuples._
  import Implicits._
  
  @Test
  def testCaseClasses {
    case class Foo(a : Int, b : String, c : Double)
    
    val f1 = Foo(23, "foo", 2.3)
    val t1 = Foo.unapply(f1).get
    val hf = t1.hlisted
    val f2 = Foo.tupled(hf)
    assertEquals(f1, f2)
  }
}
