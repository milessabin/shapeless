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
 * types of kind `*`, as well as deriving instances using an [[Iso]].
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

  /** The product containing one element. */
  final def product1[F](implicit F: C[F]): C[F :: HNil] =
    product(F, emptyProduct)

  /**The product containing two elements. */
  final def product2[F, G](implicit F: C[F], G: C[G]): C[F :: G :: HNil] =
    product(F, product(G, emptyProduct))

  /**The product containing three elements. */
  final def product3[F, G, H](implicit F: C[F], G: C[G], H: C[H]): C[F :: G :: H :: HNil] =
    product(F, product(G, product(H, emptyProduct)))


  /**
   * Given a type class instance for `G`, and an `Iso` from `F` to `G`,
   * produce a type class instance for `F`.
   */
  def derive[F, G](instance: C[G], iso: Iso[F, G]): C[F]

}

object TypeClass {

  @inline def apply[C[_]](implicit C: TypeClass[C]) = C


  // Derive type classes from isos to HLists.
  // Without the wrapper, `deriveFromIso` participates in a "diverging implicit
  // expansion" when trying to resolve the type class instance for the HList.
  // Hence, we separate that phase with a dedicated type constructor.

  class HListInstance[C[_], L <: HList](val instance: C[L]) extends AnyVal

  object HListInstance {

    implicit def nilInstance[C[_] : TypeClass]: HListInstance[C, HNil] =
      new HListInstance[C, HNil](TypeClass[C].emptyProduct)

    implicit def consInstance[C[_] : TypeClass, H, T <: HList](implicit H: C[H], T: HListInstance[C, T]): HListInstance[C, H :: T] =
      new HListInstance[C, H :: T](TypeClass[C].product(H, T.instance))

  }

  implicit def deriveFromIso[C[_] : TypeClass, F, G <: HList](implicit iso: Iso[F, G], hlistInst: HListInstance[C, G]): C[F] =
    TypeClass[C].derive(hlistInst.instance, iso)

}

// vim: expandtab:ts=2:sw=2
