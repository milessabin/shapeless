package shapeless

trait LazyScalaCompat[+T] {
  lazy val value: T
}
