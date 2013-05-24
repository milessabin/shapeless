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

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an [[Generic]].
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
   * Given a `Generic` for `F` as `Repr`, and a type class instance for `Repr`,
   * produce a type class instance for `F`.
   */
  def derive[F, Repr](instance: C[Repr], gen: GenericAux[F, Repr]): C[F]

  import TypeClass._
  
  implicit val hnilInstance: ReprInstance[C, HNil] =
    new ReprInstance[C, HNil](emptyProduct)

  implicit def hconsInstance[H, T <: HList](implicit H: C[H], T: ReprInstance[C, T]): ReprInstance[C, H :: T] =
    new ReprInstance[C, H :: T](product(H, T.instance))

  implicit def deriveFromIso[F, Repr](implicit gen: GenericAux[F, Repr], hlistInst: ReprInstance[C, Repr]): C[F] =
    derive(hlistInst.instance, gen)
}

object TypeClass {
  // classes which extend AnyVal cannot be nested
  class ReprInstance[C[_], Repr](val instance: C[Repr]) extends AnyVal
}

// vim: expandtab:ts=2:sw=2
