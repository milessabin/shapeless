import PolyFun._
import Cast._

// TODO zip/unzip
// TODO hApply
// TODO Nat/type indexed get/put
// TODO Value/type contains
// TODO take/drop
// TODO Lenses
// TODO Type-specific cases

sealed trait HList

final case class HCons[+H, +T <: HList](head : H, tail : T) extends HList {
  override def toString = head+" :: "+tail.toString
}

trait HNil extends HList {
  def ::[H](h : H) = HCons(h, this)
  override def toString = "HNil"
}

case object HNil extends HNil

trait LowPriorityHList {
  type ::[+H, +T <: HList] = HCons[H, T]

  trait Ops[L <: HList] {
  
    def head(implicit c : IsHCons[L]) : c.H 

    def tail(implicit c : IsHCons[L]) : c.T
    
    def ::[H](h : H) : H :: L
    
    def :::[P <: HList, Out <: HList](prefix : P)(implicit prepend : Prepend[P, L, Out]) : Out
  
    def reverse_:::[P <: HList, Out <: HList](prefix : P)(implicit prepend : ReversePrepend[P, L, Out]) : Out
    
    def last[Out](implicit last : Last[L, Out]) : Out

    def init[Out <: HList](implicit init : Init[L, Out]) : Out
    
    def select[U](implicit selector : Selector[L, U]) : U
    
    def reverse[Out <: HList](implicit reverse : Reverse[HNil, L, Out]) : Out
    
    def map[HF <: HRFn, Out](f : HF)(implicit mapper : Mapper[HF, L, Out]) : Out
    
    def foldLeft[R, F[_]](z : R)(f : F ~> Const[R]#λ)(op : (R, R) => R)(implicit folder : LeftFolder[L, R, F]) : R
  
    def unify[Out <: HList](implicit unifier : Unifier[L, Out]) : Out
    
    def toList[Lub](implicit l : ToList[L, Lub]) : List[Lub]
    
    def cast[M <: HList](implicit cast : Cast[L, M]) : Option[M]
  }

  implicit def hlistOps[L <: HList](l : L) : Ops[L] = new Ops[L] {

    def head(implicit c : IsHCons[L]) : c.H = c.head(l) 

    def tail(implicit c : IsHCons[L]) : c.T = c.tail(l)
    
    def ::[H](h : H) : H :: L = HCons(h, l)

    def :::[P <: HList, Out <: HList](prefix : P)(implicit prepend : Prepend[P, L, Out]) : Out = prepend(prefix, l)

    def reverse_:::[P <: HList, Out <: HList](prefix : P)(implicit prepend : ReversePrepend[P, L, Out]) : Out = prepend(prefix, l)

    def last[Out](implicit last : Last[L, Out]) : Out = last(l)

    def init[Out <: HList](implicit init : Init[L, Out]) : Out = init(l)
    
    def select[U](implicit selector : Selector[L, U]) : U = selector(l)

    def reverse[Out <: HList](implicit reverse : Reverse[HNil, L, Out]) : Out = reverse(HNil, l)

    def map[HF <: HRFn, Out](f : HF)(implicit mapper : Mapper[HF, L, Out]) : Out = mapper(f, l)
    
    def foldLeft[R, F[_]](z : R)(f : F ~> Const[R]#λ)(op : (R, R) => R)(implicit folder : LeftFolder[L, R, F]) : R = folder(l, z, f, op)

    def unify[Out <: HList](implicit unifier : Unifier[L, Out]) : Out = unifier.unify(l)
  
    def toList[Lub](implicit ll : ToList[L, Lub]) : List[Lub] = ll.toList(l)

    def cast[M <: HList](implicit cast : Cast[L, M]) : Option[M] = cast(l)
  }
  
  trait IsHCons[L <: HList] {
    type H
    type T <: HList
      
    def head(l : L) : H
    def tail(l : L) : T
  }

  implicit def hlistIsHCons[H0, T0 <: HList] = new IsHCons[H0 :: T0] {
    type H = H0
    type T = T0

    def head(l : H0 :: T0) : H = l.head
    def tail(l : H0 :: T0) : T = l.tail
  }
  
  trait Applicator[F[_], G[_], In, Out] {
    def apply(f : F ~> G, in : In) : Out
  }

  implicit def applicator1[F[_], G[_], In] = new Applicator[F, G, F[In], G[In]] {
    def apply(f : F ~> G, t : F[In]) = f(t)
  }
  
  trait Mapper[-HF <: HRFn, In, Out] {
    def apply(f : HF, in: In) : Out
  }

  implicit def hnilMapper1[F[_], G[_]] = new Mapper[F ~> G, HNil, HNil] {
    def apply(f : F ~> G, l : HNil) = HNil
  }
  
  implicit def hlistMapper1[F[_], G[_], InH, OutH, InT <: HList, OutT <: HList]
    (implicit ap : Applicator[F, G, InH, OutH], mt : Mapper[F ~> G, InT, OutT]) = new Mapper[F ~> G, InH :: InT, OutH :: OutT] {
      def apply(f : F ~> G, l : InH :: InT) = ap(f, l.head) :: mt(f, l.tail)
  }
  
  trait LeftFolder[L <: HList, R, F[_]] {
    def apply(l : L, in : R, f : F ~> Const[R]#λ, op : (R, R) => R) : R 
  }
  
  implicit def hnilLeftFolder[R, F[_]] = new LeftFolder[HNil, R, F] {
    def apply(l : HNil, in : R, f : F ~> Const[R]#λ, op : (R, R) => R) = in
  }
  
  implicit def hlistLeftFolder[H, T <: HList, R, F[_]](implicit ap : Applicator[F, Const[R]#λ, H, R], tf : LeftFolder[T, R, F]) = new LeftFolder[H :: T, R, F] {
    def apply(l : H :: T, in : R, f : F ~> Const[R]#λ, op : (R, R) => R) = tf(l.tail, op(in, ap(f, l.head)), f, op)
  }
  
  trait Lub[-A, -B, +Out] {
    def left(a : A) : Out
    def right(b : B) : Out
  }
  
  implicit def lub[T] = new Lub[T, T, T] {
    def left(a : T) : T = a
    def right(b : T) : T = b
  }
  
  trait Unifier[L <: HList, Out <: HList] {
    def unify(l : L) : Out
  }
  
  implicit def hsingleUnifier[T] = new Unifier[T :: HNil, T :: HNil] {
    def unify(l : T :: HNil) = l
  }
  
  implicit def hlistUnifier[H1, H2, L, T <: HList, Out <: HList](implicit u : Lub[H1, H2, L], lt : Unifier[L :: T, L :: Out]) = new Unifier[H1 :: H2 :: T, L :: L :: Out] {
    def unify(l : H1 :: H2 :: T) : L :: L :: Out = u.left(l.head) :: lt.unify(u.right(l.tail.head) :: l.tail.tail)
  }

  trait ToList[L <: HList, +Lub] {
    def toList(l : L) : List[Lub]
  }
  
  implicit def hsingleToList[T] : ToList[T :: HNil, T] = new ToList[T :: HNil, T] {
    def toList(l : T :: HNil) = List(l.head)
  }
  
  implicit def hlistToList[H1, H2, T <: HList, L](implicit u : Lub[H1, H2, L], ttl : ToList[H2 :: T, L]) = new ToList[H1 :: H2 :: T, L] {
    def toList(l : H1 :: H2 :: T) = u.left(l.head) :: ttl.toList(l.tail)
  }
  
  trait Last[L <: HList, Out] {
    def apply(l : L) : Out
  }
  
  implicit def hsingleLast[H] = new Last[H :: HNil, H] {
    def apply(l : H :: HNil) : H = l.head
  }
  
  implicit def hlistLast[H, T <: HList, Out](implicit lt : Last[T, Out]) = new Last[H :: T, Out] {
    def apply(l : H :: T) : Out = lt(l.tail) 
  }

  trait Init[L <: HList, Out <: HList] {
    def apply(l : L) : Out
  }
  
  implicit def hsingleInit[H] = new Init[H :: HNil, HNil] {
    def apply(l : H :: HNil) : HNil = HNil
  }
  
  implicit def hlistInit[H, T <: HList, OutH, OutT <: HList](implicit it : Init[T, OutT]) = new Init[H :: T, H :: OutT] {
    def apply(l : H :: T) : H :: OutT = l.head :: it(l.tail)
  }
  
  trait Selector[L <: HList, U] {
    def apply(l : L) : U
  }

  implicit def hlistSelect1[H, T <: HList] = new Selector[H :: T, H] {
    def apply(l : H :: T) = l.head
  }

  implicit def hlistSelect[H, T <: HList, U](implicit st : Selector[T, U]) = new Selector[H :: T, U] {
    def apply(l : H :: T) = st(l.tail)
  }

  trait Reverse[Acc <: HList, L <: HList, Out <: HList] {
    def apply(acc : Acc, l : L) : Out
  }
  
  implicit def hnilReverse[Out <: HList] = new Reverse[Out, HNil, Out] {
    def apply(acc : Out, l : HNil) : Out = acc
  }
  
  implicit def hlistReverse[Acc <: HList, InH, InT <: HList, Out <: HList](implicit rt : Reverse[InH :: Acc, InT, Out]) = new Reverse[Acc, InH :: InT, Out] {
    def apply(acc : Acc, l : InH :: InT) : Out = rt(l.head :: acc, l.tail)
  }
  
  trait Prepend[P <: HList, S <: HList, Out <: HList] {
    def apply(prefix : P, suffix : S) : Out
  }
  
  implicit def hnilPrepend[S <: HList] = new Prepend[HNil, S, S] {
    def apply(prefix : HNil, suffix : S) : S = suffix 
  }
  
  implicit def hlistPrepend[PH, PT <: HList, S <: HList, OutT <: HList](implicit pt : Prepend[PT, S, OutT]) = new Prepend[PH :: PT, S, PH :: OutT] {
    def apply(prefix : PH :: PT, suffix : S) : PH :: OutT = prefix.head :: pt(prefix.tail, suffix)
  }

  trait ReversePrepend[P <: HList, S <: HList, Out <: HList] {
    def apply(prefix : P, suffix : S) : Out
  }
  
  implicit def hnilReversePrepend[S <: HList] = new ReversePrepend[HNil, S, S] {
    def apply(prefix : HNil, suffix : S) : S = suffix 
  }
  
  implicit def hlistReversePrepend[PH, PT <: HList, S <: HList, Out <: HList](implicit rpt : ReversePrepend[PT, PH :: S, Out]) = new ReversePrepend[PH :: PT, S, Out] {
    def apply(prefix : PH :: PT, suffix : S) : Out = rpt(prefix.tail, prefix.head :: suffix)
  }
  
  trait Cast[In <: HList, Out <: HList] {
    def apply(in : In) : Option[Out] 
  }
  
  implicit def hnilCast = new Cast[HNil, HNil] {
    def apply(in : HNil) = Option(in)
  }
  
  implicit def hlistCast[InH, InT <: HList, OutH, OutT <: HList](implicit bcm : BoxedClassManifest[OutH], ct : Cast[InT, OutT]) = new Cast[InH :: InT, OutH :: OutT] {
    def apply(in : InH :: InT) : Option[OutH :: OutT] = for(h <- in.head.cast[OutH]; t <- ct(in.tail)) yield h :: t
  }
}

object HList extends LowPriorityHList {
  import PolyFun._

  implicit def applicator2[G[_], In] = new Applicator[Id, G, In, G[In]] {
    def apply(f : Id ~> G, t : In) = f(t)
  }

  implicit def applicator3[F[_], In, Out] = new Applicator[F, Const[Out]#λ, F[In], Out] {
    def apply(f : F ~> Const[Out]#λ, t : F[In]) = f(t)
  }
  
  implicit def applicator4[F[_], In] = new Applicator[F, Id, F[In], In] {
    def apply(f : F ~> Id, t : F[In]) = f(t)
  }
  
  implicit def hnilMapper2[F[_], Out] = new Mapper[F ~> Const[Out]#λ, HNil, HNil] {
    def apply(f : F ~> Const[Out]#λ, l : HNil) = HNil
  }
  
  implicit def hnilMapper3[F[_]] = new Mapper[F ~> Id, HNil, HNil] {
    def apply(f : F ~> Id, l : HNil) = HNil
  }
  
  implicit def hlistMapper2[F[_], InH, OutH, InT <: HList, OutT <: HList]
    (implicit ap : Applicator[F, Const[OutH]#λ, InH, OutH], mt : Mapper[F ~> Const[OutH]#λ, InT, OutT]) = new Mapper[F ~> Const[OutH]#λ, InH :: InT, OutH :: OutT] {
      def apply(f : F ~> Const[OutH]#λ, l : InH :: InT) = ap(f, l.head) :: mt(f, l.tail)
  }
  
  implicit def hlistMapper3[F[_], InH, OutH, InT <: HList, OutT <: HList]
    (implicit ap : Applicator[F, Id, InH, OutH], mt : Mapper[F ~> Id, InT, OutT]) = new Mapper[F ~> Id, InH :: InT, OutH :: OutT] {
      def apply(f : F ~> Id, l : InH :: InT) = ap(f, l.head) :: mt(f, l.tail)
  }
}

object TestHList {
  import HList._
  import PolyFun._
  import Traversables._

  def main(args : Array[String]) {
    val l = 1 :: "foo" :: 2.0 :: HNil
    println(l)

    type SI = Set[Int] :: HNil
    type OI = Option[Int] :: HNil

    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    val apl = implicitly[Applicator[Set, Option, Set[Int], Option[Int]]]
    val mn = implicitly[Mapper[Set ~> Option, HNil, HNil]]
    val m = implicitly[Mapper[Set ~> Option, Set[Int] :: HNil, Option[Int] :: HNil]]
    
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
    val ap : AP = a :: p :: HNil
    val bp : BP = b :: p :: HNil
    val apap : APAP = a :: p :: a :: p :: HNil
    val apbp : APBP = a :: p :: b :: p :: HNil
    val ffff : FFFF = apap
    
    val lp = apbp.last
    val lp2 : Pear = apbp.last
    val iapb = apbp.init
    val iapb2 : APB = apbp.init
    
    val pbpa = apbp.reverse
    val pbpa2 : PBPA = apbp.reverse
    println(pbpa)
    val al = a :: HNil
    val ral = al.reverse
    val ral2 : Apple :: HNil = al.reverse
    
    val apbp2 = ap ::: bp
    val apbp3 : APBP = ap ::: bp
    println(apbp2)
    
    val pabp = ap reverse_::: bp
    val pabp2 : PABP = ap reverse_::: bp
    println(pabp)
    
    val a1 : Apple = apbp2.head
    val a2 : Pear = apbp2.tail.head
    val a3 : Banana = apbp2.tail.tail.head
    val a4 : Pear = apbp2.tail.tail.tail.head
    
    def lub[X, Y, L](x : X, y : Y)(implicit lb : Lub[X, Y, L]) : (L, L) = (lb.left(x), lb.right(y))
    
    val u21 = lub(a, a)
    val u22 = lub(a, p)
    val u23 = lub(a, f)
    val u24 = lub(p, a)
    val u25 = lub(p, p)
    val u26 = lub(f, f)
    val u27 = lub(f, a)
    val u28 = lub(f, p)
    val u29 = lub(f, f)

    implicitly[Lub[HNil, HNil, HNil]]
    implicitly[Lub[Apple :: HNil, Apple :: HNil, Apple :: HNil]]
    implicitly[Lub[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Lub[Apple :: Pear :: HNil, Pear :: Apple :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Lub[ISII, IIII, IYII]]
    
    val u31 = lub(HNil, HNil)
    val u32 = lub(a :: HNil, a :: HNil)
    val u33 = lub(f :: p :: HNil, f :: f :: HNil)
    val u34 = lub(a :: p :: HNil, p :: a :: HNil)
    val u35 = lub(1 :: "two" :: 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil) 
    
    implicitly[Unifier[Apple :: HNil, Apple :: HNil]]
    implicitly[Unifier[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Unifier[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    
    implicitly[Unifier[Int :: String :: Int :: Int :: HNil, YYYY]]
    val uapap = implicitly[Unifier[Apple :: Pear :: Apple :: Pear :: HNil, FFFF]]
    
    val unified1 = uapap.unify(apap)
    val unified2 : FFFF = uapap.unify(apap)
    val unified3 = apap.unify
    val unified4 : FFFF = apap.unify
    
    val ununified1 = unified4.cast[APAP]
    println(ununified1)
    val ununified2 : Option[APAP] = unified4.cast[APAP]
    println(ununified2)
    val ununified3 = unified4.cast[APBP]
    println(ununified3)

    def getUnifier[L <: HList, Out <: HList](l : L)(implicit u : Unifier[L, Out]) = u
    
    val u2 = getUnifier(a :: HNil)
    val u3 = getUnifier(a :: a :: HNil)
    val u4 = getUnifier(a :: a :: a :: HNil)
    val u5 = getUnifier(a :: a :: a :: a :: HNil)
    val u6 = getUnifier(a :: p :: HNil)
    val u7 = getUnifier(a :: f :: HNil)
    val u8 = getUnifier(f :: a :: HNil)
    val u9a : Unifier[Apple :: Fruit :: HNil, FF] = getUnifier(a :: f :: HNil)
    val u9b : Unifier[Apple :: Pear :: HNil, FF] = getUnifier(a :: p :: HNil)
    val u10 = getUnifier(apap)
    val u11 = getUnifier(apbp)
    
    val fruits1 = apap.toList
    val fruits2 = apbp.toList
    println(fruits2)
    val fruits3 = fruits2.toHList[APBP]
    println(fruits3)
    
    val stuff = l1.toList
    println(stuff)
    val stuff2 = stuff.toHList[ISII]
    println(stuff2)
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
    val mn2 = implicitly[Mapper[Option ~> Const[Boolean]#λ, HNil, HNil]]
    val m2 = implicitly[Mapper[Option ~> Const[Boolean]#λ, Option[Int] :: HNil, Boolean :: HNil]]
    
    def blip1[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Option ~> Id, In, Out]) = ev
    val b1 = blip1(l4)
    
    def blip2[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Id ~> Option, In, Out]) = ev
    val b2 = blip2(l4)

    def blip3[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Option ~> Const[Boolean]#λ, In, Out]) = ev
    val b3 = blip3(l4)
    
    val tl1 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil 
    val tl2 = Option(1) :: Option("foo") :: (None : Option[Int]) :: Option(3) :: HNil
    
    val mlfl1 = (tl1 map isDefined).toList.foldLeft(true)(_ && _)
    println(mlfl1)
    val mlfl2 = (tl2 map isDefined).toList.foldLeft(true)(_ && _)
    println(mlfl2)
    
    val fl1 = tl1.foldLeft(true)(isDefined)(_ && _)
    println(fl1)
    val fl2 = tl2.foldLeft(true)(isDefined)(_ && _)
    println(fl2)
    
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    val si = sl.select[Int]
    println(si)
    
    val sb = sl.select[Boolean]
    println(sb)

    val ss = sl.select[String]
    println(ss)

    val sd = sl.select[Double]
    println(sd)
  }
}
