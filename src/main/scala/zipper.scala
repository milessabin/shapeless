import HList._
import Nat._

case class Zipper[L <: HList, R <: HList](prefix : L, suffix : R) {
  import Zipper._

  def right(implicit c : IsHCons[R]) = Zipper(suffix.head :: prefix, suffix.tail)

  def left(implicit c : IsHCons[L]) = Zipper(prefix.tail, prefix.head :: suffix)
  
  def rightBy[N <: Nat](implicit r : RightBy[N, L, R]) = r(prefix, suffix)

  def rightBy[N <: Nat](n : N)(implicit r : RightBy[N, L, R]) = r(prefix, suffix)

  def leftBy[N <: Nat](implicit l : LeftBy[N, L, R]) = l(prefix, suffix)

  def leftBy[N <: Nat](n : N)(implicit l : LeftBy[N, L, R]) = l(prefix, suffix)

  def rightTo[T](implicit r : RightTo[T, L, R]) = r(prefix, suffix)

  def leftTo[T](implicit l : LeftTo[T, L, R]) = l(prefix, suffix)
  
  def get(implicit c : IsHCons[R]) = suffix.head

  def put[E](e : E)(implicit c : IsHCons[R]) = Zipper(prefix, e :: suffix.tail)

  def delete(implicit c : IsHCons[R]) = Zipper(prefix, suffix.tail)
  
  def first(implicit rp : ReversePrepend[L, R]) = Zipper(HNil, prefix reverse_::: suffix)
  
  def last(implicit rp : ReversePrepend[R, L]) = Zipper(suffix reverse_::: prefix, HNil)
  
  def insert[E](e : E) = Zipper(e :: prefix, suffix)
  
  def toHList(implicit rp : ReversePrepend[L, R]) = prefix reverse_::: suffix
}

object Zipper {
  def apply[R <: HList](r : R) : Zipper[HNil, R] = Zipper(HNil, r)
  
  trait HListToZipper[L <: HList] {
    def toZipper : Zipper[HNil, L]
  }
  
  implicit def hlistToZipper[L <: HList](l : L) = new HListToZipper[L] {
    def toZipper = Zipper(l)
  }

  trait RightBy[N <: Nat, L <: HList, R <: HList] {
    type L1 <: HList
    type R1 <: HList
    def apply(prefix : L, suffix : R) : Zipper[L1, R1]
  }
  
  implicit def rightBy[N <: Nat, L <: HList, R <: HList, LP <: HList, R10 <: HList]
    (implicit split : SplitAux[R, N, LP, R10], reverse : ReversePrepend[LP, L]) =
      new RightBy[N, L, R] {
        type L1 = reverse.Out
        type R1 = R10
        def apply(prefix : L, suffix : R) : Zipper[L1, R1] = {
          val (p, s) = suffix.split[N]
          Zipper(p reverse_::: prefix, s)
        }
      }

  trait LeftBy[N <: Nat, L <: HList, R <: HList] {
    type L1 <: HList
    type R1 <: HList
    def apply(prefix : L, suffix : R) : Zipper[L1, R1]
  }

  implicit def leftBy[N <: Nat, L <: HList, R <: HList, RP <: HList, L10 <: HList]
    (implicit split : SplitAux[L, N, RP, L10], reverse : ReversePrepend[RP, R]) =
      new LeftBy[N, L, R] {
        type L1 = L10
        type R1 = reverse.Out
        def apply(prefix : L, suffix : R) : Zipper[L1, R1] = {
          val (p, s) = prefix.split[N]
          Zipper(s, p reverse_::: suffix)
        }
      }

  trait RightTo[T, L <: HList, R <: HList] {
    type L1 <: HList
    type R1 <: HList
    def apply(prefix : L, suffix : R) : Zipper[L1, R1]
  }
  
  implicit def rightTo[T, L <: HList, R <: HList, LP <: HList, R10 <: HList]
    (implicit split : SplitLeftAux[R, T, LP, R10], reverse : ReversePrepend[LP, L]) =
      new RightTo[T, L, R] {
        type L1 = reverse.Out
        type R1 = R10
        def apply(prefix : L, suffix : R) : Zipper[L1, R1] = {
          val (p, s) = suffix.splitLeft[T]
          Zipper(p reverse_::: prefix, s)
        }
      }

  trait LeftTo[T, L <: HList, R <: HList] {
    type L1 <: HList
    type R1 <: HList
    def apply(prefix : L, suffix : R) : Zipper[L1, R1]
  }

  implicit def leftTo[T, L <: HList, R <: HList, RP <: HList, R0 <: HList]
    (implicit split : SplitLeftAux[L, T, RP, R0], reverse : ReversePrepend[RP, R], cons : IsHCons[R0]) =
      new LeftTo[T, L, R] {
        type L1 = cons.T
        type R1 = cons.H :: reverse.Out
        def apply(prefix : L, suffix : R) : Zipper[L1, R1] = {
          val (p, s) = prefix.splitLeft[T]
          Zipper(s.tail, s.head :: (p reverse_::: suffix))
        }
      }
}

object TestZipper {
  import HList._
  import Nat._
  import Zipper._
  
  def main(args : Array[String]) {
    val l = 1 :: "foo" :: 3.0 :: HNil
    val z = l.toZipper
    val i = z.get
    val s = z.right.get
    val d = z.right.right.get
    
    val zl = z.last
    val d2 = zl.left.get
    val zf = zl.first
    val i2 = zf.get
    
    val l2 = l.toZipper.right.put("wibble", 45).toHList
    println(l2)
  
    val l3 = l.toZipper.right.delete.toHList
    println(l3)

    val l4 = l.toZipper.insert("bar").toHList
    println(l4)

    val l5 = l.toZipper.right.right.right.insert("bar").toHList
    println(l5)
    
    val l6 = l.toZipper.rightTo[Double]
    val d6 = l6.get
    println(l6)

    val l7 = l.toZipper.last.leftTo[Int]
    val i7 = l7.get
    println(l7)
    
    val l8 = l.toZipper.rightBy(_2)
    val d8 = l8.get
    val d8a : Double = l8.get
    
    val l9 = l8.leftBy(_1)
    val s9 = l9.get
    val s9a : String = l9.get
  }
}
