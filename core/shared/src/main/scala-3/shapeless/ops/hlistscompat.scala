/*
 * Copyright (c) 2011-16 Miles Sabin
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

package shapeless
package ops

import scala.collection.{Factory, mutable}
import scala.reflect.ClassTag

import hlist.{ToSized, ToTraversable}

// Scala 3 doesn't synthesize ClassTag[Nothing], so provide instances for empty arrays explicitly
trait ToTraversableScalaCompat {
  implicit def hnilToArrayNothing[L <: HNil]: ToTraversable.Aux[L, Array, Nothing] =
    ToTraversable.hnilToTraversable[L, Array, Nothing](ArrayOfNothing.factory)
}

trait ToSizedScalaCompat {
  implicit def hnilToSizedArrayNothing[L <: HNil]: ToSized.Aux[L, Array, Nothing, Nat._0] =
    ToSized.hnilToSized[L, Array, Nothing](ArrayOfNothing.factory, implicitly)
}

/**
 * Empty arrays as Scala 3 erases `Array[Nothing]` (ie. arrays of `scala.runtime.Nothing$`). The Scala 2.13 library's
 * `ClassTag.Nothing` creates arrays of `Object`, as Scala 2 erases them, as does Scala.js's `ArrayBuilder` for any
 * `ClassTag[Nothing]`.
 */
object ArrayOfNothing {
  val classTag: ClassTag[Nothing] = new ClassTag[Nothing] {
    def runtimeClass: Class[?] = classOf[scala.runtime.Nothing$]
    override def newArray(len: Int): Array[Nothing] = new Array[Nothing](len)
  }

  // There are no values of type Nothing, so the arrays built are always empty
  val factory: Factory[Nothing, Array[Nothing]] = new Factory[Nothing, Array[Nothing]] {
    def fromSpecific(it: IterableOnce[Nothing]): Array[Nothing] = new Array[Nothing](0)
    def newBuilder: mutable.Builder[Nothing, Array[Nothing]] = new mutable.Builder[Nothing, Array[Nothing]] {
      def addOne(elem: Nothing): this.type = this
      def clear(): Unit = ()
      def result(): Array[Nothing] = new Array[Nothing](0)
    }
  }
}
