import org.junit.Test
import org.junit.Assert._

class HListTest {
  import HList._
  import PolyFun._
  import Traversables._
  import Nat._

  def typed[T](t : => T) {}
  
  @Test
  def test {
    val l = 1 :: "foo" :: 2.0 :: HNil
    typed[Int](l.head)
    assertEquals(1, l.head)
    typed[String](l.tail.head) 
    assertEquals("foo", l.tail.head) 
    typed[Double](l.tail.tail.head)
    assertEquals(2.0, l.tail.tail.head, Double.MinPositiveValue)

    type SI = Set[Int] :: HNil
    type OI = Option[Int] :: HNil

    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    implicitly[MapperAux[choose.type, HNil, HNil]]
    implicitly[Case[choose.type, Set[Int] => Option[Int]]]
    implicitly[choose.λ[Int]]
    implicitly[MapperAux[choose.type, Set[Int] :: HNil, Option[Int] :: HNil]]
    
    val s1 = Set(1) :: HNil
    val o1 = s1 map choose
    typed[OI](o1)
    assertEquals(Option(1) :: HNil, o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 = s2 map choose
    typed[OIOS](o2)
    assertEquals(Option(1) :: Option("foo") :: HNil, o2)
    
    type ISII = Int :: String :: Int :: Int :: HNil
    type IIII = Int :: Int :: Int :: Int :: HNil
    type IYII = Int :: Any :: Int :: Int :: HNil
    type OIOSOIOI = Option[Int] :: Option[String] :: Option[Int] :: Option[Int] :: HNil
    type SISSSISI = Set[Int] :: Set[String] :: Set[Int] :: Set[Int] :: HNil

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    typed[Any :: AnyRef :: Any :: Any :: HNil](l1)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, l1)

    trait Fruit
    case class Apple() extends Fruit
    case class Pear() extends Fruit
    case class Banana() extends Fruit
    
    type YYYY = Any :: Any :: Any :: Any :: HNil
    type FF = Fruit :: Fruit :: HNil
    type AP = Apple :: Pear :: HNil
    type BP = Banana :: Pear :: HNil
    type AF = Apple :: Fruit :: HNil
    type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
    type APAP = Apple :: Pear :: Apple :: Pear :: HNil
    type APBP = Apple :: Pear :: Banana :: Pear :: HNil
    type APB = Apple :: Pear :: Banana :: HNil
    type PBPA = Pear :: Banana :: Pear :: Apple :: HNil
    type PABP = Pear :: Apple :: Banana :: Pear :: HNil
    
    val a : Apple = Apple()
    val p : Pear = Pear()
    val b : Banana = Banana()
    val f : Fruit = new Fruit {}
    
    val ap = a :: p :: HNil
    typed[AP](ap)
    val bp = b :: p :: HNil
    typed[BP](bp)
    val apap = a :: p :: a :: p :: HNil
    typed[APAP](apap)
    val apbp = a :: p :: b :: p :: HNil
    typed[APBP](apbp)
    val ffff : FFFF = apap
    typed[FFFF](ffff)
    
    val lp = apbp.last
    typed[Pear](lp)
    assertEquals(p, lp)
    
    val iapb = apbp.init
    typed[APB](iapb)
    assertEquals(a :: p :: b :: HNil, iapb)
    
    val pbpa = apbp.reverse
    typed[PBPA](pbpa)
    assertEquals(p :: b :: p :: a :: HNil, pbpa)

    val al = a :: HNil
    val ral = al.reverse
    typed[Apple :: HNil](ral)
    assertEquals(a :: HNil, ral)
    
    val apbp2 = ap ::: bp
    typed[APBP](apbp2)
    assertEquals(a :: p :: b :: p :: HNil, apbp2)

    typed[Apple](apbp2.head)
    typed[Pear](apbp2.tail.head)
    typed[Banana](apbp2.tail.tail.head)
    typed[Pear](apbp2.tail.tail.tail.head)
    
    val pabp = ap reverse_::: bp
    typed[PABP](pabp)
    assertEquals(p :: a :: b :: p :: HNil, pabp)
    
    def lub[X, Y, L](x : X, y : Y)(implicit lb : Lub[X, Y, L]) : (L, L) = (lb.left(x), lb.right(y))
    
    val u21 = lub(a, a)
    typed[(Apple, Apple)](u21)
    val u22 = lub(a, p)
    typed[(Fruit, Fruit)](u22)
    val u23 = lub(a, f)
    typed[(Fruit, Fruit)](u23)
    val u24 = lub(p, a)
    typed[(Fruit, Fruit)](u24)
    val u25 = lub(p, p)
    typed[(Pear, Pear)](u25)
    val u26 = lub(f, f)
    typed[(Fruit, Fruit)](u26)
    val u27 = lub(f, a)
    typed[(Fruit, Fruit)](u27)
    val u28 = lub(f, p)
    typed[(Fruit, Fruit)](u28)
    val u29 = lub(f, f)
    typed[(Fruit, Fruit)](u29)

    implicitly[Lub[HNil, HNil, HNil]]
    implicitly[Lub[Apple :: HNil, Apple :: HNil, Apple :: HNil]]
    implicitly[Lub[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Lub[Apple :: Pear :: HNil, Pear :: Apple :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Lub[ISII, IIII, IYII]]
    
    val u31 = lub(HNil, HNil)
    typed[(HNil, HNil)](u31)
    val u32 = lub(a :: HNil, a :: HNil)
    typed[(Apple :: HNil, Apple :: HNil)](u32)
    val u33 = lub(f :: p :: HNil, f :: f :: HNil)
    typed[(Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil)](u33)
    val u34 = lub(a :: p :: HNil, p :: a :: HNil)
    typed[(Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil)](u34)
    val u35 = lub(1 :: "two" :: 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil) 
    typed[(Int :: Any :: Int :: Int :: HNil, Int :: Any :: Int :: Int :: HNil)](u35)
    
    implicitly[UnifierAux[Apple :: HNil, Apple :: HNil]]
    implicitly[UnifierAux[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[UnifierAux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    
    implicitly[UnifierAux[Int :: String :: Int :: Int :: HNil, YYYY]]
    
    val uapap = implicitly[UnifierAux[Apple :: Pear :: Apple :: Pear :: HNil, FFFF]]
    val unified1 = uapap(apap)
    typed[FFFF](unified1)
    val unified2 = apap.unify
    typed[FFFF](unified2)
    
    val ununified1 = unified2.cast[APAP]
    assertTrue(ununified1.isDefined)
    typed[APAP](ununified1.get)
    val ununified2 = unified2.cast[APBP]
    assertFalse(ununified2.isDefined)
    typed[Option[APBP]](ununified2)

    def getUnifier[L <: HList, Out <: HList](l : L)(implicit u : UnifierAux[L, Out]) = u
    
    val u2 = getUnifier(a :: HNil)
    typed[UnifierAux[Apple :: HNil, Apple :: HNil]](u2)
    val u3 = getUnifier(a :: a :: HNil)
    typed[UnifierAux[Apple :: Apple :: HNil, Apple :: Apple :: HNil]](u3)
    val u4 = getUnifier(a :: a :: a :: HNil)
    typed[UnifierAux[Apple :: Apple :: Apple :: HNil, Apple :: Apple :: Apple :: HNil]](u4)
    val u5 = getUnifier(a :: a :: a :: a :: HNil)
    typed[UnifierAux[Apple :: Apple :: Apple :: Apple :: HNil, Apple :: Apple :: Apple :: Apple :: HNil]](u5)
    val u6 = getUnifier(a :: p :: HNil)
    //typed[UnifierAux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]](u6)
    val u7 = getUnifier(a :: f :: HNil)
    typed[UnifierAux[Apple :: Fruit :: HNil, Fruit :: Fruit :: HNil]](u7)
    val u8 = getUnifier(f :: a :: HNil)
    typed[UnifierAux[Fruit :: Apple :: HNil, Fruit :: Fruit :: HNil]](u8)
    val u9a = getUnifier(a :: f :: HNil)
    typed[UnifierAux[Apple :: Fruit :: HNil, FF]](u9a)
    val u9b = getUnifier(a :: p :: HNil)
    //typed[UnifierAux[Apple :: Pear :: HNil, FF]](u9b)
    val u10 = getUnifier(apap)
    //typed[UnifierAux[APAP, FFFF]](u10)
    val u11 = getUnifier(apbp)
    //typed[UnifierAux[APBP, FFFF]](u11)
    
    val fruits1 = apap.toList
    typed[List[Fruit]](fruits1)
    assertEquals(List(a, p, a, p), fruits1)
    
    val fruits2 = apbp.toList
    typed[List[Fruit]](fruits2)
    assertEquals(List(a, p, b, p), fruits2)
    
    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    typed[APBP](fruits3.get)
    assertEquals(apbp, fruits3.get)

    val stuff = l1.toList
    typed[List[Any]](stuff)
    assertEquals(List(1, "foo", 2, 3), stuff)
    
    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    typed[ISII](stuff2.get)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val moreStuff = (a :: "foo" :: p :: HNil).toList
    typed[List[Any]](moreStuff)
    
    val l2 = l1 map singleton
    typed[SISSSISI](l2)
    assertEquals(Set(1) :: Set("foo") :: Set(2) :: Set(3) :: HNil, l2)

    val l3 = l1 map option
    typed[OIOSOIOI](l3)
    assertEquals(Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil, l3)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    
    val l5 = l4 map get
    typed[ISII](l5)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, l5)
    
    typed[Int](l5.head)
    typed[String](l5.tail.head)
    typed[Int](l5.tail.tail.head)
    typed[Int](l5.tail.tail.tail.head)
    
    val l6 = l1 map identity
    typed[ISII](l6)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, l6)

    type BBBB = Boolean :: Boolean :: Boolean :: Boolean :: HNil
    
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals(true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.toList
    typed[Boolean](ll2.head)

    implicitly[MapperAux[isDefined.type, HNil, HNil]]
    implicitly[MapperAux[isDefined.type, Option[Int] :: HNil, Boolean :: HNil]]
    
    val tl1 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil 
    val tl2 = Option(1) :: Option("foo") :: (None : Option[Int]) :: Option(3) :: HNil
    
    val mlfl1 = (tl1 map isDefined).toList.foldLeft(true)(_ && _)
    assertTrue(mlfl1)
    val mlfl2 = (tl2 map isDefined).toList.foldLeft(true)(_ && _)
    assertFalse(mlfl2)
    
    val fl1 = tl1.foldLeft(true)(isDefined)(_ && _)
    assertTrue(fl1)
    val fl2 = tl2.foldLeft(true)(isDefined)(_ && _)
    assertFalse(fl2)
    
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
    val at0 = sn1(_0)
    typed[Int](at0)
    val at1 = sn1(_1)
    typed[Double](at1)
    val at2 = sn1(_2)
    typed[String](at2)
    val at3 = sn1(_3)
    typed[Unit](at3)
    val at4 = sn1(_4)
    typed[String](at4)
    val at5 = sn1(_5)
    typed[Boolean](at5)
    val at6 = sn1(_6)
    typed[Long](at6)

    val sni0 = sn1.split(_0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni0)
    val sni1 = sn1.split(_1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni1)
    val sni2 = sn1.split(_2)
    typed[(Int :: Double :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](sni2)
    val sni3 = sn1.split(_3)
    typed[(Int :: Double :: String :: HNil, Unit :: String :: Boolean :: Long :: HNil)](sni3)
    val sni4 = sn1.split(_4)
    typed[(Int :: Double :: String :: Unit :: HNil, String :: Boolean :: Long :: HNil)](sni4)
    val sni5 = sn1.split(_5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil, Boolean :: Long :: HNil)](sni5)
    val sni6 = sn1.split(_6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil, Long :: HNil)](sni6)
    val sni7 = sn1.split(_7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil, HNil)](sni7)
    
    val snri0 = sn1.reverse_split(_0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri0)
    val snri1 = sn1.reverse_split(_1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri1)
    val snri2 = sn1.reverse_split(_2)
    typed[(Double :: Int :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](snri2)
    val snri3 = sn1.reverse_split(_3)
    typed[(String :: Double :: Int :: HNil, Unit :: String :: Boolean :: Long :: HNil)](snri3)
    val snri4 = sn1.reverse_split(_4)
    typed[(Unit :: String :: Double :: Int :: HNil, String :: Boolean :: Long :: HNil)](snri4)
    val snri5 = sn1.reverse_split(_5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil, Boolean :: Long :: HNil)](snri5)
    val snri6 = sn1.reverse_split(_6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil, Long :: HNil)](snri6)
    val snri7 = sn1.reverse_split(_7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil, HNil)](snri7)

    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    val si = sl.select[Int]
    typed[Int](si)
    assertEquals(1, si)
    
    val sb = sl.select[Boolean]
    typed[Boolean](sb)
    assertEquals(true, sb)

    val ss = sl.select[String]
    typed[String](ss)
    assertEquals("foo", ss)

    val sd = sl.select[Double]
    typed[Double](sd)
    assertEquals(2.0, sd, Double.MinPositiveValue)
    
    val sl2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
    val (sp1, sp2) = sl.splitLeft[String]
    typed[Int :: Boolean :: HNil](sp1)
    typed[String :: Double :: HNil](sp2)
    assertEquals((sp1 ::: sp2), sl)

    val (sli1, sli2) = sl2.splitLeft[String]
    typed[Int :: Double :: HNil](sli1) 
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](sli2)
    assertEquals((sli1 ::: sli2), sl2)

    val (rsp1, rsp2) = sl.reverse_splitLeft[String]
    typed[Boolean :: Int :: HNil](rsp1)
    typed[String :: Double :: HNil](rsp2)
    assertEquals((rsp1 reverse_::: rsp2), sl)

    val (rsli1, rsli2) = sl2.reverse_splitLeft[String]
    typed[Double :: Int :: HNil](rsli1) 
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](rsli2)
    assertEquals((rsli1 reverse_::: rsli2), sl2)

    val (srp1, srp2) = sl.splitRight[String]
    typed[Int :: Boolean :: String :: HNil](srp1)
    typed[Double :: HNil](srp2)
    assertEquals((srp1 ::: srp2), sl)

    val (srli1, srli2) = sl2.splitRight[String]
    typed[Int :: Double :: String :: Unit :: String :: HNil](srli1) 
    typed[Boolean :: Long :: HNil](srli2)
    assert((srli1 ::: srli2) == sl2)

    val (rsrp1, rsrp2) = sl.reverse_splitRight[String]
    typed[String :: Boolean :: Int :: HNil](rsrp1)
    typed[Double :: HNil](rsrp2)
    assertEquals((rsrp1 reverse_::: rsrp2), sl)

    val (rsrli1, rsrli2) = sl2.reverse_splitRight[String]
    typed[String :: Unit :: String :: Double :: Int :: HNil](rsrli1) 
    typed[Boolean :: Long :: HNil](rsrli2)
    assertEquals((rsrli1 reverse_::: rsrli2), sl2)

    val l8 = 23 :: "foo" :: List(1, 2, 3, 4) :: Option("bar") :: (23, "foo") :: 2.0 :: HNil
    val l9 = l8 map size
    assertEquals(1 :: 3 :: 4 :: 4 :: 4 :: 1 :: HNil, l9)
  }
}
