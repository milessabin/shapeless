object HLists {
  
  import Rank2Poly._
  
  trait TCChecker[F[_], L <: HList]
  
  implicit def hnilTCChecker[F[_]] : TCChecker[F, HNil] = new TCChecker[F, HNil] {}
  
  implicit def hlistTCChecker[F[_], X, H, T <: HList](implicit ev1 : H <:< F[X], ev2 : TCChecker[F, T]) : TCChecker[F, H :: T] = new TCChecker[F, H :: T] {} 
  
  implicitly[TCChecker[Option, HNil]]
  implicitly[TCChecker[Option, Option[Int] :: Option[String] :: HNil]]
  //implicitly[TCChecker[Option, Int :: Option[String] :: HNil]]
  //implicitly[TCChecker[Option, Option[Int] :: String :: HNil]]
  
  /*
  trait TCStripper[F[_], L <: HList] {
    type Stripped <: HList
  }

  implicit def hnilTCStripper[F[_]] :
    TCStripper[F, HNil] { type Stripped = HNil } = new TCStripper[F, HNil] { type Stripped = HNil }

  implicit def hlistTCStripper[F[_], X, H, T <: HList](implicit ev1 : H <:< F[X], ev2 : TCStripper[F, T]) :
    TCStripper[F, H :: T] { type Stripped = X :: ev2.Stripped } = new TCStripper[F, H :: T] { type Stripped = X :: ev2.Stripped }

  val tcs = implicitly[TCStripper[Option, Option[Int] :: Option[String] :: HNil]]
  import tcs._

  val hl : Stripped = 23 :: "foo" :: HNil
  */

  sealed trait HList {
    
    type MappedTo[G[_]] <: HList
    def mapTo[G[_]](f : Id ~> G) : MappedTo[G]
    
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

    type MappedTo[G[_]] = G[H] :: T#MappedTo[G]
    def mapTo[G[_]](f : Id ~> G) : MappedTo[G] = HCons(f(head), tail.mapTo(f))

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

    type MappedTo[G[_]] = HNil
    def mapTo[G[_]](f : Id ~> G) : MappedTo[G] = HNil

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
}

object TestHList {
  import HLists._
  import Rank2Poly._
  //import Tuples._

  def main(args : Array[String]) {
    
    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    println(l1)
    
    val e11 : Int = l1.head
    val e12 : String = l1.tail.head
    val e13 : Int = l1.tail.tail.head
    val e14 : Int = l1.tail.tail.tail.head
    
    val l2 = l1 mapTo singleton
    println(l2)

    val l3 = l1 mapTo option
    println(l3)
    
    val e31 : Option[Int] = l3.head
    val e32 : Option[String] = l3.tail.head
    val e33 : Option[Int] = l3.tail.tail.head
    val e34 : Option[Int] = l3.tail.tail.tail.head

    val l4 = Option(1) :: Option("foo") :: None :: Option(3) :: HNil
    println(l4)
    
//    val l5 = l4 map get
//    println(l5)
//    val e51 : Int = l5.head
//    val e52 : String = l5.tail.head
//    val e53 : Int = l5.tail.tail.head
//    val e54 : Int = l5.tail.tail.tail.head
//    
//    val l6 = l4 map get
    
//    val b1 = l3.foldLeft(isDefined)(true)(_ & _)
//    println(b1)
//
//    val b2 = l4.foldLeft(isDefined)(true)(_ & _)
//    println(b2)
//    
//    val t6 = l1.tupled
//    val e61 : Int = t6._1
//    val e62 : String = t6._2
//    val e63 : Int = t6._3
//    val e64 : Int = t6._4
    
//    val l7 = t6.hlisted
//    val e71 : Int = l7.head
//    val e72 : String = l7.tail.head
//    val e73 : Int = l7.tail.tail.head
//    val e74 : Int = l7.tail.tail.tail.head
  }
}
