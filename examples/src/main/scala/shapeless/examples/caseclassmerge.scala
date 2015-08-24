/*
 * Copyright (c) 2015 Miles Sabin
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

import shapeless._

object CaseClassMergeDemo extends App {
  import mergeSyntax._

  case class Foo(i: Int, s: String, b: Boolean)
  case class Bar(b: Boolean, s: String)

  val foo = Foo(23, "foo", true)
  val bar = Bar(false, "bar")

  val merged = foo merge bar
  assert(merged == Foo(23, "bar", false))
}

// Implementation in terms of LabelledGeneric ...
object mergeSyntax {
  implicit class MergeSyntax[T](t: T) {
    def merge[U](u: U)(implicit merge: CaseClassMerge[T, U]): T = merge(t, u)
  }
}

trait CaseClassMerge[T, U] {
  def apply(t: T, u: U): T
}

object CaseClassMerge {
  import ops.record.Merger

  def apply[T, U](implicit merge: CaseClassMerge[T, U]): CaseClassMerge[T, U] = merge

  implicit def mkCCMerge[T, U, RT <: HList, RU <: HList]
    (implicit
      tgen: LabelledGeneric.Aux[T, RT],
      ugen: LabelledGeneric.Aux[U, RU],
      merger: Merger.Aux[RT, RU, RT]
    ): CaseClassMerge[T, U] =
    new CaseClassMerge[T, U] {
      def apply(t: T, u: U): T =
        tgen.from(merger(tgen.to(t), ugen.to(u)))
    }
}
