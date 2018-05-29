package shapeless

/**
  * Reifies implicit resolution failure as a value.
  *
  * Useful for expressing "If the implicit is available, do X. Else do Y.”
  */
final case class Perhaps[A](value: Option[A]) {
  def fold[B](ifAbsent: => B, ifPresent: A => B): B = {
    value.fold(ifAbsent)(ifPresent)
  }
}

object Perhaps {
  implicit def instance[A](implicit ev: A = null): Perhaps[A] = {
    Perhaps(Option(ev))
  }
}
