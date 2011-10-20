object HLists {
  import Rank2Poly._
  
  sealed trait HList {
    type Tupled
    type Tupled1[A]
    type Tupled2[A, B]
    type Tupled3[A, B, C]
    type Tupled4[A, B, C, D]

    def tupled : Tupled
    def tupled1[A](a : A) : Tupled1[A]
    def tupled2[A, B](a : A, b : B) : Tupled2[A, B]
    def tupled3[A, B, C](a : A, b : B, c : C) : Tupled3[A, B, C]
    def tupled4[A, B, C, D](a : A, b : B, c : C, d : D) : Tupled4[A, B, C, D]
    
    type Fn[R]
    type Fn1[A, R]
    type Fn2[A, B, R]
    type Fn3[A, B, C, R]
    type Fn4[A, B, C, D, R]
  }
  
  final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = head+" :: "+tail.toString

    def map[F[_], G[_], Out](f : F ~> G)(implicit mapper : Mapper[F, G, H :: T, Out]) : Out = mapper(f, this)
    def map[F[_], OutH, OutT <: HList](f : F ~> Const[OutH]#λ)(implicit mapper : Mapper[F, Const[OutH]#λ, H :: T, OutH :: OutT]) : OutH :: OutT = mapper(f, this)
    
    type Tupled = T#Tupled1[H]
    type Tupled1[A] = T#Tupled2[A, H]
    type Tupled2[A, B] = T#Tupled3[A, B, H]
    type Tupled3[A, B, C] = T#Tupled4[A, B, C, H]
    type Tupled4[A, B, C, D] = Nothing

    def tupled : Tupled = tail.tupled1(head)
    def tupled1[A](a : A) : Tupled1[A] = tail.tupled2(a, head)
    def tupled2[A, B](a : A, b : B) : Tupled2[A, B] = tail.tupled3(a, b, head)
    def tupled3[A, B, C](a : A, b : B, c : C) : Tupled3[A, B, C] = tail.tupled4(a, b, c, head)
    def tupled4[A, B, C, D](a : A, b : B, c : C, d : D) : Tupled4[A, B, C, D] = sys.error("boom")

    type Fn[R] = T#Fn1[H, R]
    type Fn1[A, R] = T#Fn2[A, H, R]
    type Fn2[A, B, R] = T#Fn3[A, B, H, R]
    type Fn3[A, B, C, R] = T#Fn4[A, B, C, H, R]
    type Fn4[A, B, C, D, R] = Nothing
  }

  trait HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = "HNil"

    def map[F](f : F) : HNil = HNil
    
    type Tupled = Nothing
    type Tupled1[A] = Tuple1[A]
    type Tupled2[A, B] = (A, B)
    type Tupled3[A, B, C] = (A, B, C)
    type Tupled4[A, B, C, D] = (A, B, C, D)

    def tupled : Tupled = sys.error("boom")
    def tupled1[A](a : A) : Tupled1[A] = Tuple1(a)
    def tupled2[A, B](a : A, b : B) : Tupled2[A, B] = (a, b)
    def tupled3[A, B, C](a : A, b : B, c : C) : Tupled3[A, B, C] = (a, b, c)
    def tupled4[A, B, C, D](a : A, b : B, c : C, d : D) : Tupled4[A, B, C, D] = (a, b, c, d)

    type Fn[R] = Nothing
    type Fn1[A, R] = A => R
    type Fn2[A, B, R] = (A, B) => R
    type Fn3[A, B, C, R] = (A, B, C) => R
    type Fn4[A, B, C, D, R] = (A, B, C, D) => R
  }
  
  case object HNil extends HNil
  
  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons

  trait Applicator[F[_], G[_], In, Out] {
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
  
  trait Mapper[F[_], G[_], In, Out] {
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
    type OIOSOIOI = Option[Int] :: Option[String] :: Option[Int] :: Option[Int] :: HNil
    type SISSSISI = Set[Int] :: Set[String] :: Set[Int] :: Set[Int] :: HNil

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    println(l1)
    
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

    val ap2 = implicitly[Applicator[Option, Const[Boolean]#λ, Option[Int], Boolean]]
    val mn2 = implicitly[Mapper[Option, Const[Boolean]#λ, HNil, HNil]]
    val m2 = implicitly[Mapper[Option, Const[Boolean]#λ, Option[Int] :: HNil, Boolean :: HNil]]
    
    def blip1[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Option, Id, In, Out]) = ev
    val b1 = blip1(l4)
    
    def blip2[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Id, Option, In, Out]) = ev
    val b2 = blip2(l4)

    def blip3[In <: HList, Out <: HList](in : In)(implicit ev : Mapper[Option, Const[Boolean]#λ, In, Out]) = ev
    val b3 = blip3(l4)
  }
}
