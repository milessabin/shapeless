/*
 * Copyright (c) 2013-14 Lars Hupel, Miles Sabin
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

import labelled.FieldType

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an isomorphism.
 */
trait ProductTypeClass[C[_]] extends Serializable {
  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](ch: C[H], ct: C[T]): C[H :: T]

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

trait ProductTypeClassCompanion[C[_]] extends Serializable {
  def apply[T](implicit ct: => C[T]): C[T] = ct

  val typeClass: ProductTypeClass[C]

  implicit def deriveHNil: C[HNil] = typeClass.emptyProduct

  implicit def deriveHCons[H, T <: HList](implicit ch: => C[H], ct: C[T]): C[H :: T] =
    typeClass.product(ch, ct)

  implicit def deriveInstance[F, G](implicit gen: Generic.Aux[F, G], cg: => C[G]): C[F] =
    typeClass.project(cg, gen.to, gen.from)
}


/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an isomorphism.
 * Refines ProductTypeClass with the addition of runtime `String` labels
 * corresponding to the names of the product elements.
 */
trait LabelledProductTypeClass[C[_]] extends Serializable {
  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](name: String, ch: C[H], ct: C[T]): C[H :: T]

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

trait LabelledProductTypeClassCompanion[C[_]] extends Serializable {
  def apply[T](implicit ct: => C[T]): C[T] = ct

  val typeClass: LabelledProductTypeClass[C]

  sealed abstract class Wrap[KV] extends Serializable {
    type V
    val unwrap: C[V]
    def label(values: V): KV
    def unlabel(record: KV): V
  }

  object Wrap {
    type Aux[KV, V0] = Wrap[KV] { type V = V0 }
  }

  private[shapeless] final class Instance[KV, V0](val unwrap: C[V0]) extends Wrap[KV] {
    type V = V0
    def label(values: V): KV = values.asInstanceOf[KV]
    def unlabel(record: KV): V = record.asInstanceOf[V]
  }

  implicit def deriveHNil: Wrap.Aux[HNil, HNil] =
    new Instance(typeClass.emptyProduct)

  implicit def deriveHCons[HK <: String, HV, TKV <: HList, TV <: HList](
    implicit key: Witness.Aux[HK], ch: => C[HV], ct: Wrap.Aux[TKV, TV]
  ): Wrap.Aux[FieldType[HK, HV] :: TKV, HV :: TV] =
    new Instance[FieldType[HK, HV] :: TKV, HV :: TV](
      typeClass.product(key.value, ch, ct.unwrap)
    )

  implicit def deriveInstance[T, LKV, V](
    implicit lgen: LabelledGeneric.Aux[T, LKV], wrap: => Wrap.Aux[LKV, V]
  ): C[T] = {
    val to = (t: T) => wrap.unlabel(lgen.to(t))
    val from = (v: V) => lgen.from(wrap.label(v))
    typeClass.project(wrap.unwrap, to, from)
  }
}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 */
trait TypeClass[C[_]] extends ProductTypeClass[C] {
  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](cl: => C[L], cr: => C[R]): C[L :+: R]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil]
}

trait TypeClassCompanion[C[_]] extends ProductTypeClassCompanion[C] {
  val typeClass: TypeClass[C]

  implicit def deriveCNil: C[CNil] = typeClass.emptyCoproduct

  implicit def deriveCCons[H, T <: Coproduct](implicit ch: => C[H], ct: C[T]): C[H :+: T] =
    typeClass.coproduct(ch, ct)
}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 *
 * Name hints can be safely ignored.
 */
trait LabelledTypeClass[C[_]] extends LabelledProductTypeClass[C] {
  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](name: String, cl: => C[L], cr: => C[R]): C[L :+: R]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil]
}

trait LabelledTypeClassCompanion[C[_]] extends LabelledProductTypeClassCompanion[C] {
  val typeClass: LabelledTypeClass[C]

  implicit def deriveCNil: Wrap.Aux[CNil, CNil] =
    new Instance(typeClass.emptyCoproduct)

  implicit def deriveCCons[HK <: String, HV, TKV <: Coproduct, TV <: Coproduct](
    implicit key: Witness.Aux[HK], ch: => C[HV], ct: Wrap.Aux[TKV, TV]
  ): Wrap.Aux[FieldType[HK, HV] :+: TKV, HV :+: TV] =
    new Instance[FieldType[HK, HV] :+: TKV, HV :+: TV](
      typeClass.coproduct(key.value, ch, ct.unwrap)
    )
}
