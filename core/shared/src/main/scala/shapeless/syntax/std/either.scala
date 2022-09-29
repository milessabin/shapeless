package shapeless
package syntax
package std

object either {
  implicit def eitherOps[L, R](e: Either[L, R]): EitherOps[L, R] = new EitherOps[L, R](e)
}

final class EitherOps[L, R](val e: Either[L, R]) extends AnyVal with Serializable {
  import shapeless.ops.coproduct._

  /**
    * Embeds this `Either` into a `Coproduct`
    */
  def toCoproduct(implicit eitherToCoproduct: EitherToCoproduct[L, R]): eitherToCoproduct.Out = eitherToCoproduct(e)
}
