/*
 * Copyright (c) 2013 Lars Hupel
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

import scala.language.experimental.macros

import scala.reflect.macros.Context

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an isomorphism.
 *
 * Name hints can be safely ignored.
 */
trait ProductTypeClass[C[_]] {

  final def derive[T] = macro TypeClass.derive_impl[C, T]

  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](CHead: C[H], CTail: C[T]): C[H :: T]

  /**
   * Similar to [[shapeless.ProductTypeClass#product]], but with a name hint
   * for the `H` instance. Called by [[shapeless.GenericMacros.materialize]]
   * with the name of the field with type `H`.
   */
  def namedProduct[H, T <: HList](CHead: C[H], name: String, CTail: C[T]): C[H :: T] =
    product(CHead, CTail)

  /**
   * The empty product.
   */
  def emptyProduct: C[HNil]

  /**
   * Given an isomorphism between `F` and `G`, and a type class instance for `G`,
   * produce a type class instance for `F`.
   */
  def project[F, G](instance: => C[G], to: F => G, from: G => F): C[F]

  /**
   * Name hint for a field name. Called by
   * [[shapeless.GenericMacros.materialize]] when constructing an instance for
   * a case class with just one field.
   */
  def namedField[F](instance: C[F], name: String): C[F] =
    instance

  /**
   * Name hint for a case name. Called by
   * [[shapeless.GenericMacros.materialize]] when constructing an instance for
   * an ADT with just a single case.
   */
  def namedCase[F](instance: C[F], name: String): C[F] =
    instance

}

/**
 * A type class additinally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 *
 * Name hints can be safely ignored.
 */
trait TypeClass[C[_]] extends ProductTypeClass[C] {

  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](CL: => C[L], CR: => C[R]): C[L :+: R]

  /**
   * Similar to [[shapeless.TypeClass#coproduct]], but with a name hint
   * for the `L` instance. Called by [[shapeless.GenericMacros.materialize]]
   * with the name of the case class associated with type `L`.
   */
  def namedCoproduct[L, R <: Coproduct](CL: => C[L], name: String, CR: => C[R]): C[L :+: R] =
    coproduct(CL, CR)

  /**
   * Given a type class instances for `L`, produce a type class instance
   * for the coproduct `L :+: CNil`. Called by
   * [[shapeless.GenericMacros.materialize]] when constructing the first
   * instance in a coproduct.
   *
   * Since `CNil` is uninhabited, there is a safe default implementation.
   * Overriding can be useful for performance reasons.
   */
  def coproduct1[L](CL: => C[L]): C[L :+: CNil] = {
    def from(l: L): L :+: CNil = Inl(l)
    def to(cp: L :+: CNil) = cp match {
      case Inl(l) => l
      case Inr(_) => sys.error("absurd")
    }
    project(CL, to, from)
  }

  /**
   * Similar to [[shapeless.TypeClass#coproduct1]], but with a name hint
   * for the `L` instance. Called by [[shapeless.GenericMacros.materialize]]
   * with the name of the case class associated with type `L`.
   */
  def namedCoproduct1[L](CL: => C[L], name: String): C[L :+: CNil] =
    coproduct1(CL)

}

trait TypeClassCompanion[C[_]] {
  object auto {
    implicit def derive[T] = macro TypeClass.derive_impl[C, T]
  }
}

final class IgnoreParent

object TypeClass {

  implicit def ignoreParent: IgnoreParent = new IgnoreParent()

  def apply[C[_], T] = macro derive_impl[C, T]

  def derive_impl[C[_], T](context: Context)(implicit tTag: context.WeakTypeTag[T], cTag: context.WeakTypeTag[C[Any]]): context.Expr[C[T]] = {
    val helper = new GenericMacros.Helper[context.type] {
      val c: context.type = context
      val expandInner = true
      val optimizeSingleItem = true
      val checkParent = true
      val tpe = tTag.tpe
    }
    context.Expr[C[T]](helper.ADT.deriveInstance(cTag.tpe.typeConstructor))
  }

}

// vim: expandtab:ts=2:sw=2
