/*
 * Copyright (c) 2012-14 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.examples

/**
 * Flattening arbitrarily nested tuples.
 *
 * @author Miles Sabin
 */
object FlattenExample {
  import shapeless._
  import ops.tuple.FlatMapper
  import syntax.std.tuple._
  import test._
  
  trait LowPriorityFlatten extends Poly1 {
    implicit def default[T] = at[T](Tuple1(_))
  }
  object flatten extends LowPriorityFlatten {
    implicit def caseTuple[P <: Product](implicit lfm: Lazy[FlatMapper[P, flatten.type]]) =
      at[P](lfm.value(_))
  }

  val t1 = (1, ((2, 3), 4))
  val f1 = flatten(t1)     // Inferred type is (Int, Int, Int, Int)
  val l1 = f1.toList       // Inferred type is List[Int]
  typed[List[Int]](l1)
  
  object toDouble extends Poly1 {
    implicit def caseInt = at[Int](_.toDouble)
    implicit def caseDouble = at[Double](identity)
  }
  
  val t2 = (1, ((2, 3.0), 4))
  val f2 = flatten(t2)     // Inferred type is (Int, Int, Double, Int)
  val ds = f2 map toDouble // Inferred type is (Double, Double, Double, Double)
  val l2 = ds.toList       // Inferred type is List[Double]
  typed[List[Double]](l2)
  
  val t3 = (23, ((true, 2.0, "foo"), "bar"), (13, false))
  val f3 = flatten(t3)     // Inferred type is (Int, Boolean, Double, String, String, Int, Boolean)
  typed[(Int, Boolean, Double, String, String, Int, Boolean)](f3)
}
