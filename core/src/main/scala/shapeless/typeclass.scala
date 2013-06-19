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
 */
trait TypeClass[C[_]] {

  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](CHead: C[H], CTail: C[T]): C[H :: T]

  /**
   * The empty product.
   */
  def emptyProduct: C[HNil]

  /**
   * Given an isomorphism between `F` and `G`, and a type class instance for `G`,
   * produce a type class instance for `F`.
   */
  def project[F, G](instance: => C[G], to: F => G, from: G => F): C[F]

}

/**
 * A type class abstracting additionally over the `coproduct` operation of type
 * classes over types of kind `*`.
 */
trait TypeClassWithCoproduct[C[_]] extends TypeClass[C] {

  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](CL: => C[L], CR: => C[R]): C[L :+: R]

}

trait TypeClassCompanion[C[_]] {
  object auto {
    implicit def derive[T] = macro TypeClass.derive_impl[C, T]
  }
}

object TypeClass {

  def apply[C[_], T] = macro derive_impl[C, T]

  def derive_impl[C[_], T](context: Context)(implicit tTag: context.WeakTypeTag[T], cTag: context.WeakTypeTag[C[Any]]): context.Expr[C[T]] = {
    val helper = new GenericMacros.Helper[context.type] {
      val c: context.type = context
      val expandInner = true
      val optimizeSingleItem = true
      val tpe = tTag.tpe
    }
    context.Expr[C[T]](helper.ADT.deriveInstance(cTag.tpe.typeConstructor))
  }

}

// vim: expandtab:ts=2:sw=2
