package shapeless.examples

  object klist {
  import shapeless._
  import HList._
  
  /**
   * Type class witnessing that every element of L has TC as its outer type constructor 
   */
  trait OuterTyCon[L <: HList, TC[_]]
  object OuterTyCon {
    implicit def hnilOuterTyCon[TC[_]] = new OuterTyCon[HNil, TC] {}
    implicit def hlistOuterTyCon[H, T <: HList, TC[_]](implicit otct : OuterTyCon[T, TC]) =
      new OuterTyCon[Option[H] :: T, TC] {}
  }
  
  // Function which will only accept HList's whose elements all have have Option as their
  // outer type constructor of
  def acceptOption[L <: HList](l : L)(implicit ev : OuterTyCon[L, Option]) = true
  
  val l1 = Option(23) :: Option(true) :: Option("foo") :: HNil 
  val l2 = Option(23) :: true :: Option("foo") :: HNil
  
  acceptOption(l1)  // Compiles
  //acceptOption(l2)  // Does not compile
}
