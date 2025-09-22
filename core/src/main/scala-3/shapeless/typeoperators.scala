package shapeless

trait theScalaCompat {
  transparent inline def apply[T](using x: T): x.type = x
}
