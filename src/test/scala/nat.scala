import org.junit.Test
import org.junit.Assert._

class NatTests {
  import Nat._
  
  @Test
  def testNat {
    implicitly[Succ[_1] =:= _2]
    implicitly[Sum[_2, _3, _5]]
    
    implicitly[Pred[_19, _18]]
    
    implicitly[Prod[_2, _3, _6]]
    implicitly[Prod[_4, _5, _20]]
  }
}
