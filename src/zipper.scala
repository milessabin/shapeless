import HList._

case class Zipper[L <: HList, R <: HList](prefix : L, suffix : R) {
  import Zipper._

  def right(implicit c : IsHCons[R]) = Zipper(suffix.head :: prefix, suffix.tail)

  def left(implicit c : IsHCons[L]) = Zipper(prefix.tail, prefix.head :: suffix)
  
  def rightTo[T](implicit r : RightTo[T, L, R]) = r(prefix, suffix)

  def leftTo[T](implicit l : LeftTo[T, L, R]) = l(prefix, suffix)
  
  def get(implicit c : IsHCons[R]) = suffix.head

  def put[E](e : E)(implicit c : IsHCons[R]) = Zipper(prefix, e :: suffix.tail)

  def delete(implicit c : IsHCons[R]) = Zipper(prefix, suffix.tail)
  
  def first[Out <: HList](implicit rp : ReversePrepend[L, R, Out]) = Zipper(HNil, prefix reverse_::: suffix)
  
  def last[Out <: HList](implicit rp : ReversePrepend[R, L, Out]) = Zipper(suffix reverse_::: prefix, HNil)
  
  def insert[E](e : E) = Zipper(e :: prefix, suffix)
  
  def toHList[Out <: HList](implicit rp : ReversePrepend[L, R, Out]) = prefix reverse_::: suffix
}

object Zipper {
  def apply[R <: HList](r : R) : Zipper[HNil, R] = Zipper(HNil, r)
  
  trait HListToZipper[L <: HList] {
    def toZipper : Zipper[HNil, L]
  }
  
  implicit def hlistToZipper[L <: HList](l : L) = new HListToZipper[L] {
    def toZipper = Zipper(l)
  }

  trait RightTo[T, ZL <: HList, ZR <: HList] {
    type L <: HList
    type R <: HList
    def apply(prefix : ZL, suffix : ZR) : Zipper[L, R]
  }
  
  implicit def rightTo[T, ZL <: HList, ZR <: HList, P <: HList, S <: HList, Out <: HList]
    (implicit split : SplitLeft[HNil, ZR, T, P, S], reverse : ReversePrepend[P, ZL, Out]) =
      new RightTo[T, ZL, ZR] {
        type L = Out 
        type R = S
        def apply(prefix : ZL, suffix : ZR) : Zipper[L, R] = {
          val (p, s) = suffix.splitLeft[T]
          Zipper(p reverse_::: prefix, s)
        }
      }

  trait LeftTo[T, ZL <: HList, ZR <: HList] {
    type L <: HList
    type R <: HList
    def apply(prefix : ZL, suffix : ZR) : Zipper[L, R]
  }

  implicit def leftTo[T, ZL <: HList, ZR <: HList, P <: HList, S <: HList, Out <: HList]
    (implicit split : SplitLeft[HNil, ZL, T, P, S], reverse : ReversePrepend[P, ZR, Out], cons : IsHCons[S]) =
      new LeftTo[T, ZL, ZR] {
        type L = cons.T
        type R = cons.H :: Out
        def apply(prefix : ZL, suffix : ZR) : Zipper[L, R] = {
          val (p, s) = prefix.splitLeft[T]
          Zipper(s.tail, s.head :: (p reverse_::: suffix))
        }
      }
}

object TestZipper {
  import HList._
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
  }
}
