import org.junit.Test
import org.junit.Assert._

class LiftOFnTests {
  import PolyFun._
  import HList._
  import Tuples._
  import Functions._
  import LiftOFn._
  
  @Test
  def testLiftO {
    
    val sum : (Int, Int) => Int = _ + _
    val prd : (Int, Int, Int) => Int = _ * _ * _
    
    val hlsum = sum.hlisted
    val hlprd = prd.hlisted
    
    val l1 = 2 :: 3 :: HNil
    val l2 = 2 :: 3 :: 4 :: HNil
    
    val s1 = hlsum(l1)
    assertTrue(s1 == 5)
    
    val p1 = hlprd(l2)
    assertTrue(p1 == 24)
    
    val l3 = Option(2) :: Option(3) :: HNil
    val isDef3 = l3.foldLeft(true)(isDefined)(_ & _)
    assertTrue(isDef3)
    val l3a = l3 map get
    val s2a = hlsum(l3a)
    assertTrue(s2a == 5)

    val l4 = Option(2) :: Option(3) :: Option(4) :: HNil
    val isDef4 = l4.foldLeft(true)(isDefined)(_ & _)
    assertTrue(isDef4)
    val l4a = l4 map get
    val p2a = hlprd(l4a)
    assertTrue(p2a == 24)
    
    import Functions.Implicits._
    
    val sumO = liftO(sum)

    val s2 = sumO(Some(1), Some(2))
    assertTrue(s2.isDefined)
    assertTrue(s2.get == 3)
    

    val s3 = sumO(Some(1), None)
    assertTrue(s3.isEmpty)
    
    val s4 = sumO(None, Some(2))
    assertTrue(s4.isEmpty)
    
    val s5 = sumO(None, None)
    assertTrue(s5.isEmpty)
    
    val s6 = List(Some(1), Some(2), Some(3), Some(4)).reduce(sumO)
    assertTrue(s6.isDefined)
    assertTrue(s6.get == 10)
    
    val prdO = liftO(prd)

    val p2 = prdO(Some(2), Some(3), Some(4))
    assertTrue(p2.isDefined)
    assertTrue(p2.get == 24)

    val p3 = prdO(Some(2), None, Some(4))
    assertTrue(p3.isEmpty)

    val p4 = prdO(Some(2), Some(3), None)
    assertTrue(p4.isEmpty)
  }
}
