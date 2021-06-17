package shapeless

trait LazyScalaCompat[+T] {
  val value: T
}
