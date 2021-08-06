package shapeless

import scala.util.NotGiven

/** Evidence that no implicit instance of type `T` is available
 *
 *  @author Zainab Ali
 */
@annotation.implicitNotFound(msg = "Implicit instance for ${T} in scope.")
trait Refute[T]
object Refute {

  /** This always declares an instance of `Refute`
   *
   * This instance will only be found when there is no evidence of `T`
   * */
  implicit def refute[T](implicit dummy: NotGiven[T]): Refute[T] = new Refute[T] {}
}
