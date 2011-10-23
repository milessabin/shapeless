object HLists {
  import Rank2Poly._
  
  sealed trait HList
  
  final case class HCons[+H, +T <: HList](head : H, tail : T) extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = head+" :: "+tail.toString
    
    def map[F[_], G[_], Out](f : F ~> G)(implicit mapper : Mapper[F, G, H :: T, Out]) : Out = mapper(f, this)
    def map[F[_], OutH, OutT <: HList](f : F ~> Const[OutH]#λ)(implicit mapper : Mapper[F, Const[OutH]#λ, H :: T, OutH :: OutT]) : OutH :: OutT = mapper(f, this)
    
    def unify[Out <: HList](implicit unifier : Unifier[H, T, Out]) = unifier.unify(this)

    def toList[Lub](implicit l : ToList[H, T, Lub]) : List[Lub] = l.toList(this)
  }
  
  trait HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = "HNil"
      
    def map[F](f : F) : HNil = HNil
    
    def unify : HNil = HNil
    
    def toList = Nil
  }
  
  case object HNil extends HNil
  
  type ::[+H, +T <: HList] = HCons[H, T]
  val :: = HCons
  
  trait Applicator[F[_], G[_], -In, +Out] {
    def apply(f : F ~> G, in : In) : Out
  }

  implicit def applicator1[F[_], G[_], In] = new Applicator[F, G, F[In], G[In]] {
    def apply(f : F ~> G, t : F[In]) = f(t)
  }
  
  implicit def applicator2[G[_], In] = new Applicator[Id, G, In, G[In]] {
    def apply(f : Id ~> G, t : In) = f(t)
  }

  implicit def applicator3[F[_], In, Out] = new Applicator[F, Const[Out]#λ, F[In], Out] {
    def apply(f : F ~> Const[Out]#λ, t : F[In]) = f(t)
  }
  
  trait Mapper[F[_], G[_], -In, +Out] {
    def apply(f : F ~> G, in: In) : Out
  }

  implicit def hnilMapper1[F[_], G[_]] = new Mapper[F, G, HNil, HNil] {
    def apply(f : F ~> G, l : HNil) = HNil
  }
  
  implicit def hnilMapper2[F[_], Out] = new Mapper[F, Const[Out]#λ, HNil, HNil] {
    def apply(f : F ~> Const[Out]#λ, l : HNil) = HNil
  }
  
  implicit def hlistMapper1[F[_], G[_], InH, OutH, InT <: HList, OutT <: HList]
    (implicit ap : Applicator[F, G, InH, OutH], mt : Mapper[F, G, InT, OutT]) = new Mapper[F, G, InH :: InT, OutH :: OutT] {
      def apply(f : F ~> G, l : InH :: InT) = HCons(ap(f, l.head), mt(f, l.tail))
  }
  
  implicit def hlistMapper2[F[_], InH, OutH, InT <: HList, OutT <: HList]
    (implicit ap : Applicator[F, Const[OutH]#λ, InH, OutH], mt : Mapper[F, Const[OutH]#λ, InT, OutT]) = new Mapper[F, Const[OutH]#λ, InH :: InT, OutH :: OutT] {
      def apply(f : F ~> Const[OutH]#λ, l : InH :: InT) = HCons(ap(f, l.head), mt(f, l.tail))
  }
  
  trait Unify2[-A, -B, +Out] {
    def left(a : A) : Out
    def right(b : B) : Out
  }
  
  implicit def unify2[T] = new Unify2[T, T, T] {
    def left(a : T) : T = a
    def right(b : T) : T = b
  }
  
  trait Unifier[-H, -T <: HList, +Out <: HList] {
    def unify(l : H :: T) : Out
  }
  
  implicit def hsingleUnifier[T] = new Unifier[T, HNil, T :: HNil] {
    def unify(l : T :: HNil) = l
  }
  
  implicit def hlistUnifier[H1, H2, L, T <: HList, Out <: HList](implicit u : Unify2[H1, H2, L], lt : Unifier[L, T, L :: Out]) = new Unifier[H1, H2 :: T, L :: L :: Out] {
    def unify(l : H1 :: H2 :: T) : L :: L :: Out = u.left(l.head) :: lt.unify(HCons(u.right(l.tail.head), l.tail.tail))
  }

  trait ToList[-H, -T <: HList, +Lub] {
    def toList(l : H :: T) : List[Lub]
  }
  
  implicit def hsingleToList[T] : ToList[T, HNil, T] = new ToList[T, HNil, T] {
    def toList(l : T :: HNil) = Nil
  }
  
  implicit def hlistToList[H1, H2, T <: HList, L](implicit u : Unify2[H1, H2, L], ttl : ToList[H2, T, L]) = new ToList[H1, H2 :: T, L] {
    def toList(l : H1 :: H2 :: T) = u.left(l.head) :: ttl.toList(l.tail)
  }
}

object TestHList {
  import HLists._
  import Rank2Poly._
  //import Tuples._

  def main(args : Array[String]) {
    type SI = Set[Int] :: HNil
    type OI = Option[Int] :: HNil

    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    val ap = implicitly[Applicator[Set, Option, Set[Int], Option[Int]]]
    val mn = implicitly[Mapper[Set, Option, HNil, HNil]]
    val m = implicitly[Mapper[Set, Option, Set[Int] :: HNil, Option[Int] :: HNil]]
    
    val s1 = Set(1) :: HNil
    val o1 = s1 map choose
    val o1b : OI = s1 map choose

    println(s1)
    println(o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 = s2 map choose
    val o2b : OIOS = s2 map choose
    
    println(s2)
    println(o2)

    type ISII = Int :: String :: Int :: Int :: HNil
    type IIII = Int :: Int :: Int :: Int :: HNil
    type IYII = Int :: Any :: Int :: Int :: HNil
    type OIOSOIOI = Option[Int] :: Option[String] :: Option[Int] :: Option[Int] :: HNil
    type SISSSISI = Set[Int] :: Set[String] :: Set[Int] :: Set[Int] :: HNil

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    val l1b : Any :: AnyRef :: Any :: Any :: HNil = l1
    println(l1)

    trait Fruit
    trait Apple extends Fruit
    trait Pear extends Fruit
    trait Banana extends Fruit
    
    type YYYY = Any :: Any :: Any :: Any :: HNil
    type FF = Fruit :: Fruit :: HNil
    type AP = Apple :: Pear :: HNil
    type AF = Apple :: Fruit :: HNil
    type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
    type APAP = Apple :: Pear :: Apple :: Pear :: HNil
    type APBP = Apple :: Pear :: Banana :: Pear :: HNil
    
    val a : Apple = new Apple {}
    val p : Pear = new Pear {}
    val b : Banana = new Banana {}
    val f : Fruit = new Fruit {}
    val apap : APAP = a :: p :: a :: p :: HNil
    val apbp : APBP = a :: p :: b :: p :: HNil
    val ffff : FFFF = apap
    
    def unify[A, B, C](a : A, b : B)(implicit u : Unify2[A, B, C]) : (C, C) = (u.left(a), u.right(b))
    
    val u21 = unify(a, a)
    val u22 = unify(a, p)
    val u23 = unify(a, f)
    val u24 = unify(p, a)
    val u25 = unify(p, p)
    val u26 = unify(f, f)
    val u27 = unify(f, a)
    val u28 = unify(f, p)
    val u29 = unify(f, f)

    implicitly[Unify2[HNil, HNil, HNil]]
    implicitly[Unify2[Apple :: HNil, Apple :: HNil, Apple :: HNil]]
    implicitly[Unify2[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Unify2[Apple :: Pear :: HNil, Pear :: Apple :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Unify2[ISII, IIII, IYII]]
    
    val u31 = unify(HNil, HNil)
    val u32 = unify(a :: HNil, a :: HNil)
    val u33 = unify(f :: p :: HNil, f :: f :: HNil)
    val u34 = unify(a :: p :: HNil, p :: a :: HNil)
    val u35 = unify(1 :: "two" :: 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil) 
    
    implicitly[Unifier[Apple, HNil, Apple :: HNil]]
    implicitly[Unifier[Fruit, Pear :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Unifier[Apple, Pear :: HNil, Fruit :: Fruit :: HNil]]
    
    implicitly[Unifier[Int, String :: Int :: Int :: HNil, YYYY]]
    val uapap = implicitly[Unifier[Apple, Pear :: Apple :: Pear :: HNil, FFFF]]
    
    val unified1 = uapap.unify(apap)
    val unified2 : FFFF = uapap.unify(apap)
    val unified3 = apap.unify
    val unified4 : FFFF = apap.unify

    def getUnifier[H, T <: HList, Out <: HList](l : H :: T)(implicit u : Unifier[H, T, Out]) = u
    
    val u2 = getUnifier(a :: HNil)
    val u3 = getUnifier(a :: a :: HNil)
    val u4 = getUnifier(a :: a :: a :: HNil)
    val u5 = getUnifier(a :: a :: a :: a :: HNil)
    val u6 = getUnifier(a :: p :: HNil)
    val u7 = getUnifier(a :: f :: HNil)
    val u8 = getUnifier(f :: a :: HNil)
    val u9a : Unifier[Apple, Fruit :: HNil, FF] = getUnifier(a :: f :: HNil)
    val u9b : Unifier[Apple, Pear :: HNil, FF] = getUnifier(a :: p :: HNil)
    val u10 = getUnifier(apap)
    val u11 = getUnifier(apbp)
    
    val fruits1 = apap.toList
    val fruits2 = apbp.toList
    val stuff = l1.toList
    val moreStuff = (a :: "foo" :: p :: HNil).toList
    
    val l2 /*: SISSSISI */ = l1 map singleton
    val l2b : SISSSISI = l1 map singleton
    println(l2)

    val l3 /*: OIOSOIOI */ = l1 map option
    val l3b : OIOSOIOI = l1 map option
    println(l3)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    println(l4)
    
    val l5 /*: ISII */ = l4 map get
    val l5b : ISII = l4 map get
    println(l5)
    
    val e51 : Int = l5.head
    val e52 : String = l5.tail.head
    val e53 : Int = l5.tail.tail.head
    val e54 : Int = l5.tail.tail.tail.head
    
    val l6 /*: ISII */ = l1 map identity
    val l6b : ISII = l1 map identity
    println(l6)

    type BBBB = Boolean :: Boolean :: Boolean :: Boolean :: HNil
    
    val l7 /*: BBBB */ = l4 map isDefined
    val l7b : BBBB = l4 map isDefined
    println(l7)

    val ll2 = l7.toList
    val bh : Boolean = ll2.head

    val ap2 = implicitly[Applicator[Option, Const[Boolean]#λ, Option[Int], Boolean]]
    val mn2 = implicitly[Mapper[Option, Const[Boolean]#λ, HNil, HNil]]
    val m2 = implicitly[Mapper[Option, Const[Boolean]#λ, Option[Int] :: HNil, Boolean :: HNil]]
    
    def blip1[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Option, Id, In, Out]) = ev
    val b1 = blip1(l4)
    
    def blip2[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Id, Option, In, Out]) = ev
    val b2 = blip2(l4)

    def blip3[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Option, Const[Boolean]#λ, In, Out]) = ev
    val b3 = blip3(l4)
    
    val tl1 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil 
    val tl2 = Option(1) :: Option("foo") :: None :: Option(3) :: HNil
    
    assert((tl1 map isDefined).toList.foldLeft(true)(_ && _))
    assert(!(tl2 map isDefined).toList.foldLeft(true)(_ && _))
  }
}
