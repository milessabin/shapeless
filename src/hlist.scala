object HLists {
  import Rank2Poly._
  
  sealed trait HList
  
  final case class HCons[+H, +T <: HList](head : H, tail : T) extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = head+" :: "+tail.toString
    
    def map[F[_], G[_], Out](f : F ~> G)(implicit mapper : Mapper[F, G, H :: T, Out]) : Out = mapper(f, this)
    def map[F[_], OutH, OutT <: HList](f : F ~> Const[OutH]#λ)(implicit mapper : Mapper[F, Const[OutH]#λ, H :: T, OutH :: OutT]) : OutH :: OutT = mapper(f, this)
    
    def toList[Lub](implicit l : ToList[H :: T, Lub]) : List[Lub] = l.toList(this)
    
    def unify[Out <: HList](implicit unifier : Unifier[H :: T, Out]) = unifier.unify(this)
  }
  
  trait HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = "HNil"
      
    def map[F](f : F) : HNil = HNil
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
  
  trait Bounds[+B, -L]
  
  implicit def hnilBounds[X] = new Bounds[X, HNil] {}
  
  implicit def hlistBounds[X, H, T <: HList](implicit ev : H <:< X, tb : Bounds[X, T]) = new Bounds[X, H :: T] {} 
  
  trait Repeats[L <: HList, X, Out <: HList]
  
  implicit def hnilRepeats[X] = new Repeats[HNil, X, HNil] {}
  
  implicit def hlistRepeats[H, T <: HList, X, OutT <: HList](implicit rep : Repeats[T, X, OutT]) = new Repeats[H :: T, X, X :: OutT] {}
  
  trait Unify2[-A, -B, +Out] {
    def left(a : A) : Out
    def right(b : B) : Out
  }
  
  implicit def unify2[T] = new Unify2[T, T, T] {
    def left(a : T) : T = a
    def right(b : T) : T = b
  }
  
  trait Lub[-H, -T <: HList, +L]
  
  implicit def hsingleLub[T] = new Lub[T, HNil, T] {}
  
  implicit def hlistLub[H1, H2, L1, T <: HList, L2](implicit u : Unify2[H1, H2, L1], lt : Lub[L1, T, L2]) = new Lub[H1, H2 :: T, L2] {} 

  trait Unifier[-In, +Out] {
    def unify(l : In) : Out
  }
  
  implicit def hnilUnifier : Unifier[HNil, HNil] = new Unifier[HNil, HNil] {
    def unify(l : HNil) = l
  }
  
  implicit def hlistUnifier[H, T <: HList, L, Out <: HList](implicit l : Lub[H, T, L], r : Repeats[H :: T, L, Out]) = new Unifier[H :: T, Out] {
    def unify(l : H :: T) : Out = l.asInstanceOf[Out]
  }
  trait ToList[-L, +Lub] {
    def toList(l : L) : List[Lub]
  }
  
  implicit def hnilToList : ToList[HNil, Nothing] = new ToList[HNil, Nothing] {
    def toList(l : HNil) = Nil
  }
  
  implicit def hlistToList[H, T <: HList, L](implicit lb : Lub[H, T, L], ttl : ToList[T, L]) = new ToList[H :: T, L] {
    def toList(l : H :: T) = l.head.asInstanceOf[L] :: ttl.toList(l.tail)
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
    
    type YYYY = Any :: Any :: Any :: Any :: HNil
    type FF = Fruit :: Fruit :: HNil
    type AP = Apple :: Pear :: HNil
    type AF = Apple :: Fruit :: HNil
    type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
    type APAP = Apple :: Pear :: Apple :: Pear :: HNil
    
    implicitly[Bounds[Any, ISII]]
    implicitly[Bounds[Fruit, APAP]] 
    
    val a : Apple = new Apple {}
    val p : Pear = new Pear {}
    val f : Fruit = new Fruit {}
    val apap : APAP = a :: p :: a :: p :: HNil
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
    
    implicitly[Lub[Nothing, HNil, Nothing]]
    implicitly[Lub[Apple, HNil, Apple]]
    implicitly[Lub[Fruit, Pear :: HNil, Fruit]]
    implicitly[Lub[Pear, Fruit :: HNil, Fruit]]
    implicitly[Lub[Apple, Pear :: HNil, Fruit]]
    implicitly[Lub[Pear, Apple :: HNil, Fruit]]
    implicitly[Lub[Apple, Pear :: Apple :: Pear :: HNil, Fruit]]
    implicitly[Lub[Pear, Apple :: Pear :: Apple :: HNil, Fruit]]

    implicitly[Unifier[HNil, HNil]]
    implicitly[Unifier[Apple :: HNil, Apple :: HNil]]
    implicitly[Unifier[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Unifier[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    
    implicitly[Unifier[ISII, YYYY]]
    val uapap = implicitly[Unifier[APAP, FFFF]]
    
    val unified1 = uapap.unify(apap)
    val unified2 : FFFF = uapap.unify(apap)
    val unified3 = apap.unify
    val unified4 : FFFF = apap.unify

    def getUnifier[In <: HList, Out <: HList](l : In)(implicit u : Unifier[In, Out]) = u
    
    val u1 = getUnifier(HNil)
    val u2 = getUnifier(a :: HNil)
    val u3 = getUnifier(a :: a :: HNil)
    val u4 = getUnifier(a :: a :: a :: HNil)
    val u5 = getUnifier(a :: a :: a :: a :: HNil)
    val u6 = getUnifier(a :: p :: HNil)
    val u7 = getUnifier(a :: f :: HNil)
    val u8 = getUnifier(f :: a :: HNil)
    val u9a : Unifier[AF, FF] = getUnifier(a :: f :: HNil)
    val u9b : Unifier[AP, FF] = getUnifier(a :: p :: HNil)
    val u10 = getUnifier(apap)
    
    val fruits = apap.toList
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
    val b : Boolean = ll2.head

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
