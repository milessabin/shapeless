package shapeless

import org.junit.Test
import org.junit.Assert._

class NMapTests {
  import NMap._
  import HList._
  import Typeable._
  
  def typed[T](t : => T) {}

  @Test
  def testNMap {
    val m = Map((1 :: true :: "foo" :: HNil, 2.0), (2 :: true :: "foo" :: HNil, 3.0))
    
    val mi = m(1)
    typed[Map[Boolean :: String :: HNil, Double]](mi)
    assertEquals(Map((true :: "foo" :: HNil, 2.0)), mi)
    
    val mb = m(true)
    typed[Map[Int :: String :: HNil, Double]](mb)
    assertEquals(Map((1 :: "foo" :: HNil, 2.0), (2 :: "foo" :: HNil, 3.0)), mb)
    
    val mib = m(1 :: true :: HNil)
    typed[Map[String :: HNil, Double]](mib)
    assertEquals(Map(("foo" :: HNil, 2.0)), mib)
  }
}