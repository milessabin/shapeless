package shapeless

import scala.reflect.ClassTag

trait HListTestsScalaCompat { this: HListTests =>

  type PWS = Fruit

  given ClassTag[Nothing] = ClassTag.Nothing

  type MIntStringDoubleBound = M[_ >: String & (Int & Double) <: String | (Int | Double)] 
  type M2IntStringDoubleBound[A] = M2[_ >: String & (Int & Double) <: String | (Int | Double), A] 
  type AnyOrMatchable = Matchable
}
