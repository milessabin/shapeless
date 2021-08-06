package shapeless

trait theScalaCompat {
  transparent inline def apply[T](using inline x: T): x.type = x
}
