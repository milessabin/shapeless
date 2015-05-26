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

/**
 * Functional update of common fields of a sealed family of case classes
 * via a case-class-like copy through the common super type ...
 */
object BaseCopyDemo extends App {
  import copySyntax._

  // Sealed family of case classes ...
  sealed trait Base
  case class Foo(i: Int, b: Boolean) extends Base
  case class Bar(i: Int, s: String, b: Boolean) extends Base
  case class Baz(i: Int, b: Boolean, d: Double) extends Base
  case class Quux(c: Char, i: Int, b: Boolean) extends Base

  // case class copy style functional update through the common super-type ...
  val b1: Base = Foo(23, true)
  assert(b1.copy(i = 13) == Foo(13, true))

  val b2: Base = Bar(23, "foo", false)
  assert(b2.copy(i = 13, b = true) == Bar(13, "foo", true))

  val b3: Base = Baz(23, false, 2.3)
  assert(b3.copy(i = 13) == Baz(13, false, 2.3))

  val b4: Base = Quux('*', 23, false)
  assert(b4.copy(b = true, i = 13) == Quux('*', 13, true))
}

/**
 * Functional update of common fields of an open family of case classes
 * via a case-class-like copy through the common super type ...
 */
object OpenBaseCopyDemo extends App {
  import openCopySyntax._
  import mergeSyntax._

  // Open family of case classes ...
  trait Base extends OpenFamily[Base] {
    val i: Int
    val b: Boolean

    case class BaseFields(i: Int, b: Boolean)
    def baseFields = BaseFields(i, b)
    def baseCopy(base: BaseFields): Base
  }

  case class Foo(i: Int, b: Boolean) extends Base {
    def baseCopy(base: BaseFields) = this merge base
  }

  case class Bar(i: Int, s: String, b: Boolean) extends Base {
    def baseCopy(base: BaseFields) = this merge base
  }

  case class Baz(i: Int, b: Boolean, d: Double) extends Base {
    def baseCopy(base: BaseFields) = this merge base
  }

  case class Quux(c: Char, i: Int, b: Boolean) extends Base {
    def baseCopy(base: BaseFields) = this merge base
  }


  // case class copy style functional update through the common super-type ...
  val b1: Base = Foo(23, true)
  assert(b1.copy(i = 13) == Foo(13, true))

  val b2: Base = Bar(23, "foo", false)
  assert(b2.copy(i = 13, b = true) == Bar(13, "foo", true))

  val b3: Base = Baz(23, false, 2.3)
  assert(b3.copy(i = 13) == Baz(13, false, 2.3))

  val b4: Base = Quux('*', 23, false)
  assert(b4.copy(b = true, i = 13) == Quux('*', 13, true))
}

// Implementation in terms of RecordArgs, Generic and Lazy ...
object copySyntax {
  class CopySyntax[T](t: T) {
    object copy extends RecordArgs {
      def applyRecord[R <: HList](r: R)(implicit update: UpdateRepr[T, R]): T = update(t, r)
    }
  }

  implicit def apply[T](t: T): CopySyntax[T] = new CopySyntax[T](t)
}

object openCopySyntax {
  class CopySyntax[T, BaseFields0](t: OpenFamily[T] { type BaseFields = BaseFields0 }) {
    object copy extends RecordArgs {
      def applyRecord[R <: HList](r: R)(implicit update: UpdateRepr[BaseFields0, R]): T =
        t.baseCopy(update(t.baseFields, r))
    }
  }

  implicit def apply[T](t: OpenFamily[T]): CopySyntax[T, t.BaseFields] = new CopySyntax(t)
}

trait OpenFamily[T] {
  type BaseFields
  def baseFields: BaseFields
  def baseCopy(base: BaseFields): T
}

trait UpdateRepr[T, R <: HList] {
  def apply(t: T, r: R): T
}

object UpdateRepr {
  import ops.record._

  implicit def mergeUpdateRepr[T <: HList, R <: HList]
    (implicit merger: Merger.Aux[T, R, T]): UpdateRepr[T, R] =
    new UpdateRepr[T, R] {
      def apply(t: T, r: R): T = merger(t, r)
    }

  implicit def cnilUpdateRepr[R <: HList]: UpdateRepr[CNil, R] =
    new UpdateRepr[CNil, R] {
      def apply(t: CNil, r: R): CNil = t
    }

  implicit def cconsUpdateRepr[H, T <: Coproduct, R <: HList]
    (implicit
      uh: Lazy[UpdateRepr[H, R]],
      ut: Lazy[UpdateRepr[T, R]]
    ): UpdateRepr[H :+: T, R] =
    new UpdateRepr[H :+: T, R] {
      def apply(t: H :+: T, r: R): H :+: T = t match {
        case Inl(h) => Inl(uh.value(h, r))
        case Inr(t) => Inr(ut.value(t, r))
      }
    }

  implicit def genProdUpdateRepr[T, R <: HList, Repr <: HList]
    (implicit
      prod: HasProductGeneric[T],
      gen: LabelledGeneric.Aux[T, Repr],
      update: Lazy[UpdateRepr[Repr, R]]
    ): UpdateRepr[T, R] =
    new UpdateRepr[T, R] {
      def apply(t: T, r: R): T = gen.from(update.value(gen.to(t), r))
    }

  implicit def genCoprodUpdateRepr[T, R <: HList, Repr <: Coproduct]
    (implicit
      coprod: HasCoproductGeneric[T],
      gen: Generic.Aux[T, Repr],
      update: Lazy[UpdateRepr[Repr, R]]
    ): UpdateRepr[T, R] =
    new UpdateRepr[T, R] {
      def apply(t: T, r: R): T = gen.from(update.value(gen.to(t), r))
    }
}
