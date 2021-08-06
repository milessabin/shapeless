package shapeless

import scala.reflect.ClassTag

trait HListTestsScalaCompat { this: HListTests =>

  type PWS = Fruit

  given ClassTag[Nothing] = ClassTag.Nothing

  type IntStringDoubleBound >: String & (Int & Double) <: String | (Int | Double)
  type AnyOrMatchable = Matchable
}
