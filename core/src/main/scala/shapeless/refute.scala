package shapeless

/** Evidence that no implicit instance of type `T` is available
  *
  *  @author Zainab Ali
  */
@annotation.implicitNotFound(msg = "Implicit instance for ${T} in scope.")
trait Refute[T]

object Refute {

  trait Aux[A]
  object Aux {
    /** This results in  ambigous implicits if there is implicit evidence of `T` */
    implicit def amb1[T](implicit ev: T): Aux[T] = null
    implicit def amb2[T]: Aux[T] = null
  }

  /** This always declares an instance of `Refute`
    *
    * This instance will only be found when there is no evidence of `T`
    * */
  implicit def refute[T](implicit dummy: Aux[T]): Refute[T] = new Refute[T] {}
}
