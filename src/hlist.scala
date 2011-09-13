object HLists {
  
  import Rank2Poly._
  
  object HList {
    implicit def hlistToTuple[F[_], H <: HList[F]](h : H) : H#Tupled = h.tupled
  }
  
  sealed trait HList[F[_]] {
    type Mapped[G[_]] <: HList[G]
    def map[G[_]](f : F ~> G) : Mapped[G]
    
    def foldLeft[R](f : F ~> Const[R]#λ)(z : R)(acc : (R, R) => R) : R
    
    type Tupled
    type Tupled1[A]
    type Tupled2[A, B]
    type Tupled3[A, B, C]
    type Tupled4[A, B, C, D]

    def tupled : Tupled
    def tupled1[A](a : F[A]) : Tupled1[A]
    def tupled2[A, B](a : F[A], b : F[B]) : Tupled2[A, B]
    def tupled3[A, B, C](a : F[A], b : F[B], c : F[C]) : Tupled3[A, B, C]
    def tupled4[A, B, C, D](a : F[A], b : F[B], c : F[C], d : F[D]) : Tupled4[A, B, C, D]
    
    type Fn[R]
    type Fn1[A, R]
    type Fn2[A, B, R]
    type Fn3[A, B, C, R]
    type Fn4[A, B, C, D, R]
  }
  
  object HCons {
    trait HConsBuilder[F[_]] {
      def apply[H, T <: HList[F]](head : F[H], tail : T) = new HCons[F, H, T](head, tail)
    }
    def apply[F[_]] = new HConsBuilder[F]{}
  }

  final case class HCons[F[_], H, T <: HList[F]](head : F[H], tail : T) extends HList[F] {
    def ::[T](v : F[T]) = HCons(v, this)
    override def toString = head+" :: "+tail.toString

    type Mapped[G[_]] = HCons[G, H , T#Mapped[G]]
    def map[G[_]](f : F ~> G) : Mapped[G] = HCons[G](f(head), tail map f)

    def foldLeft[R](f : F ~> Const[R]#λ)(z : R)(acc : (R, R) => R) : R = tail.foldLeft(f)(acc(z, f(head)))(acc)

    type Tupled = T#Tupled1[H]
    type Tupled1[A] = T#Tupled2[A, H]
    type Tupled2[A, B] = T#Tupled3[A, B, H]
    type Tupled3[A, B, C] = T#Tupled4[A, B, C, H]
    type Tupled4[A, B, C, D] = Nothing

    def tupled : Tupled = tail.tupled1(head)
    def tupled1[A](a : F[A]) : Tupled1[A] = tail.tupled2(a, head)
    def tupled2[A, B](a : F[A], b : F[B]) : Tupled2[A, B] = tail.tupled3(a, b, head)
    def tupled3[A, B, C](a : F[A], b : F[B], c : F[C]) : Tupled3[A, B, C] = tail.tupled4(a, b, c, head)
    def tupled4[A, B, C, D](a : F[A], b : F[B], c : F[C], d : F[D]) : Tupled4[A, B, C, D] = sys.error("boom")

    type Fn[R] = T#Fn1[H, R]
    type Fn1[A, R] = T#Fn2[A, H, R]
    type Fn2[A, B, R] = T#Fn3[A, B, H, R]
    type Fn3[A, B, C, R] = T#Fn4[A, B, C, H, R]
    type Fn4[A, B, C, D, R] = Nothing
  }

  final class HNil[F[_]]() extends HList[F] {
    def ::[T](v : F[T]) = HCons(v, this)
    override def toString = "HNil"

    type Mapped[G[_]] = HNil[G]
    def map[G[_]](f : F ~> G) : Mapped[G] = HNil[G]

    def foldLeft[R](f : F ~> Const[R]#λ)(z : R)(acc : (R, R) => R) : R = z

    type Tupled = Nothing
    type Tupled1[A] = Tuple1[F[A]]
    type Tupled2[A, B] = (F[A], F[B])
    type Tupled3[A, B, C] = (F[A], F[B], F[C])
    type Tupled4[A, B, C, D] = (F[A], F[B], F[C], F[D])

    def tupled : Tupled = sys.error("boom")
    def tupled1[A](a : F[A]) : Tupled1[A] = Tuple1(a)
    def tupled2[A, B](a : F[A], b : F[B]) : Tupled2[A, B] = (a, b)
    def tupled3[A, B, C](a : F[A], b : F[B], c : F[C]) : Tupled3[A, B, C] = (a, b, c)
    def tupled4[A, B, C, D](a : F[A], b : F[B], c : F[C], d : F[D]) : Tupled4[A, B, C, D] = (a, b, c, d)

    type Fn[R] = Nothing
    type Fn1[A, R] = F[A] => R
    type Fn2[A, B, R] = (F[A], F[B]) => R
    type Fn3[A, B, C, R] = (F[A], F[B], F[C]) => R
    type Fn4[A, B, C, D, R] = (F[A], F[B], F[C], F[D]) => R
  }
  
  object HNil {
    def apply[F[_]] = new HNil[F]
  }
  
  type ::[H, T <: HList[Id]] = HCons[Id, H, T]
  val :: = HCons
}

object TestHList {
  import HLists._
  import Rank2Poly._
  import Tuples._

  def main(args : Array[String]) {
    
    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil[Id]
    println(l1)
    
    val e11 : Int = l1.head
    val e12 : String = l1.tail.head
    val e13 : Int = l1.tail.tail.head
    val e14 : Int = l1.tail.tail.tail.head
    
    val l2 = l1 map singleton
    println(l2)
    
    val l3 = l1 map option
    println(l3)
    
    val e31 : Option[Int] = l3.head
    val e32 : Option[String] = l3.tail.head
    val e33 : Option[Int] = l3.tail.tail.head
    val e34 : Option[Int] = l3.tail.tail.tail.head

    val l4 = Option(1) :: Option("foo") :: None :: Option(3) :: HNil[Option]
    println(l4)
    
    val l5 = l3 map get
    val e51 : Int = l5.head
    val e52 : String = l5.tail.head
    val e53 : Int = l5.tail.tail.head
    val e54 : Int = l5.tail.tail.tail.head
    
    val b1 = l3.foldLeft(isDefined)(true)(_ & _)
    println(b1)

    val b2 = l4.foldLeft(isDefined)(true)(_ & _)
    println(b2)
    
    val t6 = l1.tupled
    val e61 : Int = t6._1
    val e62 : String = t6._2
    val e63 : Int = t6._3
    val e64 : Int = t6._4
    
    val l7 = t6.hlisted
    val e71 : Int = l7.head
    val e72 : String = l7.tail.head
    val e73 : Int = l7.tail.tail.head
    val e74 : Int = l7.tail.tail.tail.head
  }
}
