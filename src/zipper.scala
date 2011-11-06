import HList._

case class Zipper[L <: HList, R <: HList](prefix : L, suffix : R) {

  def right(implicit c : IsHCons[R]) = Zipper(suffix.head :: prefix, suffix.tail)

  def left(implicit c : IsHCons[L]) = Zipper(prefix.tail, prefix.head :: suffix)
  
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
  }
}
