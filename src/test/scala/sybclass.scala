class SybClassTests {
  import org.junit.{ Ignore, Test }
  import org.junit.Assert._

  import Data._
  import Trans._
  import GSizeAll._
  import GSizeAll2._
  import Everything._
  import IncAll._
  import IncAll2._
  import Everywhere._

  @Test
  def testGMapQ {
    val p = (23, "foo")
    val ps = gsizeAll(p)
    assertEquals(5, ps)

    val l = List(1, 2, 3) 
    val ls = gsizeAll(l)
    assertEquals(4, ls)

    val lp = List(("foo", 23), ("bar", 24))
    val lps = gsizeAll(lp)
    assertEquals(11, lps)
  }

  @Test
  def testGMapT {
    val p = (23, "foo")
    val pi = incAll(p)
    assertEquals((24, "foo*"), pi)

    val o = Option(23)
    val oi = incAll(o)
    assertEquals(Some(24), oi)

    val e : Either[String, Int] = Right(23)
    val ei = incAll(e)
    assertEquals(Right(24), ei)

    val lo = List(Some(1), None, Some(2))
    val loi = incAll(lo)
    assertEquals(List(Some(2), None, Some(3)), loi)
  }

  @Ignore @Test
  def testEverything {
    val p = (23, "foo")
    val ps = gsizeAll2(p)
    assertEquals(5, ps)

    val l = List(1, 2, 3) 
    val ls = gsizeAll2(l)
    assertEquals(4, ls)

    val lp = List(("foo", 23), ("bar", 24))
    val lps = gsizeAll2(lp)
    assertEquals(11, lps)
  }

  @Test
  def testEverywhere {
    val p = (23, "foo")
    val pi = incAll2(p)
    assertEquals((24, "foo*"), pi)

    val o = Option(23)
    val oi = incAll2(o)
    assertEquals(Some(24), oi)

    val e : Either[String, Int] = Right(23)
    val ei = incAll2(e)
    assertEquals(Right(24), ei)

    val lo = List(Some(1), None, Some(2))
    val loi = incAll2(lo)
    assertEquals(List(Some(2), None, Some(3)), loi)

    val e1 = everywhere(inc)(23)
    assertEquals(24, e1)

    val e2 = everywhere(inc)(Option(23))
    assertEquals(Option(24), e2)
      
    val e3 = everywhere(inc)(List(23))
    assertEquals(List(24), e3)
        
    val e4 = everywhere(inc)(List(Option(23)))
    assertEquals(List(Option(24)), e4)
          
    val e5 = everywhere(inc)(Option(List(23)))
    assertEquals(Option(List(24)), e5)

    val e6 = everywhere(inc)(List(List(List(23))))
    assertEquals(List(List(List(24))), e6)

    val e7 = everywhere(inc)(Option(Option(Option(23))))
    assertEquals(Option(Option(Option(24))), e7)

    val e8 = everywhere(inc)(List(Option(List(Option(List(Option(23)))))))
    assertEquals(List(Option(List(Option(List(Option(24)))))), e8)
  }
}
