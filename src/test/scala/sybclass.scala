class SybClassTests {
  import org.junit.{ Ignore, Test }
  import org.junit.Assert._

  import SybClass._
  import PolyFun._
  import HList._

  def typed[T](t : => T) {}
  
  object gsizeAll extends (Id ~> Const[Int]#λ) with NoDefault
  implicit def gsizeAllString = gsizeAll.λ[String](s => s.length)
  implicit def gsizeAllDflt[T](implicit data : Data[gsizeAll.type, T]) = gsizeAll.λ[T](1+data.gmapQ(_).sum) 

  object gsize extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = 1
  }
  implicit def gsizeInt = gsize.λ[Int](i => 1)
  implicit def gsizeString = gsize.λ[String](s => s.length)
  
  def gsizeAll2[T](t : T)(implicit e : Everything[gsize.type, T]) : Int = everything(gsize)(_+_)(t) 

  object incAll extends (Id ~> Id) with NoDefault
  implicit def incAllInt = incAll.λ[Int](_+1)
  implicit def incAllString = incAll.λ[String](_+"*")
  implicit def incAllDflt[T](implicit data : DataT[incAll.type, T]) = incAll.λ[T](data.gmapT)

  object inc extends (Id ~> Id) {
    def default[T](t : T) = t
  }
  implicit def incInt = inc.λ[Int](_+1)
  implicit def incString = inc.λ[String](_+"*")

  def incAll2[T](t : T)(implicit e : Everywhere[inc.type, T]) : T = everywhere(inc)(t)
  
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

  @Test
  def testEverything {
    val e1 = everything(gsize)(_+_)(23)
    typed[Int](e1)
    assertEquals(1, e1)
    
    val e2 = everything(gsize)(_+_)("foo")
    typed[Int](e2)
    assertEquals(3, e2)
    
    val e3 = everything(gsize)(_+_)((23, "foo"))
    typed[Int](e3)
    assertEquals(5, e3)

    val e4 = everything(gsize)(_+_)(List(1, 2, 3, 4))
    typed[Int](e4)
    assertEquals(5, e4)
    
    val is = gsizeAll2(23)
    typed[Int](is)
    assertEquals(1, is)

    val ss = gsizeAll2("foo")
    typed[Int](ss)
    assertEquals(3, ss)

    val ps = gsizeAll2(23, "foo")
    typed[Int](ps)
    assertEquals(5, ps)

    val ls = gsizeAll2(List(1, 2, 3, 4))
    typed[Int](ls)
    assertEquals(5, ls)

    val lps = gsizeAll2(List(("foo", 23), ("bar", 24)))
    typed[Int](lps)
    assertEquals(11, lps)
  }

  @Test
  def testEverywhere {
    val pi = incAll2((23, "foo"))
    typed[(Int, String)](pi)
    assertEquals((24, "foo*"), pi)

    val oi = incAll2(Option(23))
    typed[Option[Int]](oi)
    assertEquals(Some(24), oi)

    val ei = incAll2(Right(23) : Either[String, Int])
    typed[Either[String, Int]](ei)
    assertEquals(Right(24), ei)

    val loi = incAll2(List(Some(1), None, Some(2)))
    typed[List[Option[Int]]](loi)
    assertEquals(List(Some(2), None, Some(3)), loi)

    val e1 = everywhere(inc)(23)
    typed[Int](e1)
    assertEquals(24, e1)

    val e2 = everywhere(inc)(Option(23))
    typed[Option[Int]](e2)
    assertEquals(Option(24), e2)
      
    val e3 = everywhere(inc)(List(23))
    typed[List[Int]](e3)
    assertEquals(List(24), e3)
        
    val e4 = everywhere(inc)(List(Option(23)))
    typed[List[Option[Int]]](e4)
    assertEquals(List(Option(24)), e4)
          
    val e5 = everywhere(inc)(Option(List(23)))
    typed[Option[List[Int]]](e5)
    assertEquals(Option(List(24)), e5)

    val e6 = everywhere(inc)(List(List(List(23))))
    typed[List[List[List[Int]]]](e6)
    assertEquals(List(List(List(24))), e6)

    val e7 = everywhere(inc)(Option(Option(Option(23))))
    typed[Option[Option[Option[Int]]]](e7)
    assertEquals(Option(Option(Option(24))), e7)

    val e8 = everywhere(inc)(List(Option(List(Option(List(Option(23)))))))
    typed[List[Option[List[Option[List[Option[Int]]]]]]](e8)
    assertEquals(List(Option(List(Option(List(Option(24)))))), e8)
  }
  
  @Test
  def testHList {
    val l = 23 :: "foo" :: true :: 2.0 :: HNil
    
    val li = everywhere(inc)(l)
    typed[Int :: String :: Boolean :: Double :: HNil](li)
    assertEquals(24 :: "foo*" :: true :: 2.0 :: HNil, li)
    
    val ls = everything(gsize)(_+_)(l)
    typed[Int](ls)
    assertEquals(7, ls)
  }
}
