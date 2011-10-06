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

  trait Applicator[H, T, R] {
    def apply(t : T) : R
  }

  implicit def applicator1[F[_], G[_], H <: F ~> G, T](f : H) : Applicator[H, F[T], G[T]] = new Applicator[H, F[T], G[T]] {
    def apply(t : F[T]) : G[T] = f(t)
  }

  implicit def applicator2[G[_], H <: Id ~> G, T](f : H) : Applicator[H, T, G[T]] = new Applicator[H, T, G[T]] {
    def apply(t : T) : G[T] = f(t)
  }

  implicit def applicator3[F[_], H <: F ~> Id, T](f : H) : Applicator[H, F[T], T] = new Applicator[H, F[T], T] {
    def apply(t : F[T]) : T = f(t)
  }

  trait Mapper[H, -In, +Out] {
    def map(f : H)(t : In) : Out
  }
  
  implicit def hnilMapper[H] = new Mapper[H, HNil, HNil] {
    def map(f : H)(l : HNil) = HNil
  }
  
  implicit def hlistMapper[H, InH, OutH, InT <: HList, OutT <: HList](implicit ap : H => Applicator[H, InH, OutH], mt : Mapper[H, InT, OutT]) = new Mapper[H, InH :: InT, OutH :: OutT] {
    def map(f : H)(l : InH :: InT) = HCons(ap(f)(l.head), mt.map(f)(l.tail))
  }

  def map[H, In <: HList, Out <: HList](f : H)(in : In)(implicit mapper : Mapper[H, In, Out]) : Out = mapper.map(f)(in)
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
    
    val s1 = Set(1) :: HNil
    val o1 : OI = map(choose)(s1)

    println(s1)
    println(o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 : OIOS = map(choose)(s2)
    
    println(s2)
    println(o2)

    type ISII = Int :: String :: Int :: Int :: HNil
    type OIOSOIOI = Option[Int] :: Option[String] :: Option[Int] :: Option[Int] :: HNil
    type SISSSISI = Set[Int] :: Set[String] :: Set[Int] :: Set[Int] :: HNil

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    println(l1)
    
    val l2 : SISSSISI = map(singleton)(l1)
    println(l2)

    val l3 : OIOSOIOI = map(option)(l1)
    println(l3)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    println(l4)
    
    val l5 : ISII = map(get)(l4)
    println(l5)
    
    val e51 : Int = l5.head
    val e52 : String = l5.tail.head
    val e53 : Int = l5.tail.tail.head
    val e54 : Int = l5.tail.tail.tail.head
  }
}
