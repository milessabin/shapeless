package shapeless

/** Evidence that no implicit instance of type `T` is available
  *
  *  @author Zainab Ali
  */
trait Refute[T]

object Refute {

  /** This results in  ambigous implicits if there is implicit evidence of `T` */
  implicit def ambiguousIfPresent[T](implicit ev: T): Refute[T] = new Refute[T] {}

  /** This always declares an instance of `Refute`
    *
    * This instance will only be found when there is no evidence of `T`
    * */
  implicit def refute[T](implicit dummy: DummyImplicit): Refute[T] = new Refute[T] {}
}
