import HList._

case class Zipper[L <: HList, R <: HList](prefix : L, suffix : R) {
  def first[Out <: HList](implicit rp : ReversePrepend[L, R, Out]) = Zipper(HNil, prefix reverse_::: suffix)
  
  def last[Out <: HList](implicit rp : ReversePrepend[R, L, Out]) = Zipper(suffix reverse_::: prefix, HNil)
  
  def insert[E](e : E) = Zipper(HCons(e, prefix), suffix)
  
  def toHList[Out <: HList](implicit rp : ReversePrepend[L, R, Out]) = prefix reverse_::: suffix
}

object Zipper {
  def apply[R <: HList](r : R) : Zipper[HNil, R] = Zipper(HNil, r)
  
  trait ZipperRight[L <: HList, RH, RT <: HList] {
    def right : Zipper[RH :: L, RT]
  }
  
  implicit def zipperRight[L <: HList, RH, RT <: HList](z : Zipper[L, RH :: RT]) = new ZipperRight[L, RH, RT] {
    def right = Zipper(HCons(z.suffix.head, z.prefix), z.suffix.tail)
  }
  
  trait ZipperLeft[LH, LT <: HList, R <: HList] {
    def left : Zipper[LT, LH :: R]
  }
  
  implicit def zipperLeft[LH, LT <: HList, R <: HList](z : Zipper[LH :: LT, R]) = new ZipperLeft[LH, LT, R] {
    def left = Zipper(z.prefix.tail, HCons(z.prefix.head, z.suffix))
  }
  
  trait ZipperGet[L <: HList, RH, RT <: HList] {
    def get : RH
  }
  
  implicit def zipperGet[L <: HList, RH, RT <: HList](z : Zipper[L, RH :: RT]) = new ZipperGet[L, RH, RT] {
    def get : RH = z.suffix.head
  }
  
  trait ZipperPut[L <: HList, RH, RT <: HList] {
    def put[E](e : E) : Zipper[L, E :: RT]
  }
  
  implicit def zipperPut[L <: HList, RH, RT <: HList](z : Zipper[L, RH :: RT]) = new ZipperPut[L, RH, RT] {
    def put[E](e : E) = Zipper(z.prefix, HCons(e, z.suffix.tail))
  }
  
  trait ZipperDelete[L <: HList, RT <: HList] {
    def delete : Zipper[L, RT]
  }
  
  implicit def zipperDelete[L <: HList, RH, RT <: HList](z : Zipper[L, RH :: RT]) = new ZipperDelete[L, RT] {
    def delete = Zipper(z.prefix, z.suffix.tail)
  }
  
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