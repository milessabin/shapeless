class SybClassTests {
  import org.junit.{ Ignore, Test }
  import org.junit.Assert._

  import SybClass._
  import PolyFun._

  object gsizeAll extends (Id ~> Const[Int]#λ) with NoDefault
  implicit def gsizeAllString = gsizeAll.λ[String](s => s.length)
  implicit def gsizeAllDflt[T](implicit data : Data[gsizeAll.type, T, Int]) = gsizeAll.λ[T](1+data.gmapQ(_).sum) 

  object gsize extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = 1
  }
  implicit def gsizeInt = gsize.λ[Int](i => i*2)
  implicit def gsizeString = gsize.λ[String](s => s.length)
  
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
    println(gmapQ(gsize)(23))
    println(gmapQ(gsize)("foo"))
    println(gmapQ(gsize)((23, "foo")))
    println(gmapQ(gsize)(List(1, 2, 3, 4)))
    
    
//    val e1 = everything(gsize)
//    val e2 = e1(_+_)
//  
//    e2(23)
//    e2("foo")
//    e2((23, "foo"))
  
  //  def gsizeAll2[T](t : T)(implicit e : Everything[gsize.type, Int, T]) : Int = {
//    val sum : (Int, Int) => Int = _+_ 
//    val eg = everything(gsize)
//    eg(sum, t)
//  }

//    val ci = implicitly[Everything[gsize.type, Int, Int]]
//    implicitly[Case[gsize.type, Int => Int]]
//    implicitly[Data[Everything0[gsize.type, Int], Int, Int]]
//    
//    val xx = ci.f(_+_, 23)
//    println("xx: ", xx)
//    
//    val cs = implicitly[Everything[gsize.type, Int, String]]
//    val xx2 = cs.f(_+_, "foo")
//    println("xx2: ", xx2)
//
//    val cp = implicitly[Everything[gsize.type, Int, (Int, String)]]
//    implicitly[Case[gsize.type, ((Int, String)) => Int]]
//
//    val xx3 = cp.f(_+_, (23, "foo"))
//    println("xx3: ", xx3)
//
//    val deg = implicitly[Data[Everything0[gsize.type, Int], (Int, String), Int]]
//    val xx4 = deg.gmapQ((23, "foo"))
//    println("xx4: ", xx4)
//
//    val i = 23
//    val is = gsizeAll2(i)
//    assertEquals(1, is)
//
//    val s = "foo"
//    val ss = gsizeAll2(s)
//    assertEquals(3, ss)
//
//    val p = (23, "foo")
//    val ps = gsizeAll2(p)
//    assertEquals(5, ps)
//
//    val l = List(1, 2, 3) 
//    val ls = gsizeAll2(l)
//    assertEquals(4, ls)
//
//    val lp = List(("foo", 23), ("bar", 24))
//    val lps = gsizeAll2(lp)
//    assertEquals(11, lps)
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

    val eei = everywhere(inc)

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
