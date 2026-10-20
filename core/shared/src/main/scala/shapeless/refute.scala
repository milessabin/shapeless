package shapeless

/** Evidence that no implicit instance of type `T` is available
  *
  *  @author Zainab Ali
  */
@annotation.implicitNotFound(msg = "Implicit instance for ${T} in scope.")
trait Refute[T]

object Refute extends RefuteScalaCompat
