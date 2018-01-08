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

import labelled.{ field, FieldType }

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
  def apply[T](implicit ct: Lazy[C[T]]): C[T] = ct.value

  val typeClass: ProductTypeClass[C]

  implicit def deriveHNil: C[HNil] = typeClass.emptyProduct

  implicit def deriveHCons[H, T <: HList] (implicit ch: Lazy[C[H]], ct: Lazy[C[T]]): C[H :: T] =
    typeClass.product(ch.value, ct.value)

  implicit def deriveInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[C[G]]): C[F] =
    typeClass.project(cg.value, gen.to _, gen.from _)
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
  def apply[T](implicit ct: Lazy[C[T]]): C[T] = ct.value

  val typeClass: LabelledProductTypeClass[C]

  trait Wrap[KV] extends Serializable {
    type V
    val unwrap: C[V]
    def label(v: V): KV
    def unlabel(rec: KV): V
  }

  object Wrap {
    type Aux[KV, V0] = Wrap[KV] { type V = V0 }
  }

  implicit def deriveHNil: Wrap.Aux[HNil, HNil] =
    new Wrap[HNil] {
      type V = HNil
      val unwrap = typeClass.emptyProduct
      def label(v: HNil): HNil = HNil
      def unlabel(rec: HNil): HNil = HNil
    }

  implicit def deriveHCons[HK <: Symbol, HV, TKV <: HList]
    (implicit
      ch: Lazy[C[HV]],
      key: Witness.Aux[HK],
      ct: Lazy[Wrap[TKV] { type V <: HList }]
    ): Wrap.Aux[FieldType[HK, HV] :: TKV, HV :: ct.value.V] =
      new Wrap[FieldType[HK, HV] :: TKV] {
        type V = HV :: ct.value.V
        val unwrap = typeClass.product(key.value.name, ch.value, ct.value.unwrap)
        def label(v: HV :: ct.value.V): FieldType[HK, HV] :: TKV = field[HK](v.head) :: ct.value.label(v.tail)
        def unlabel(rec: FieldType[HK, HV] :: TKV): HV :: ct.value.V = rec.head :: ct.value.unlabel(rec.tail)
      }

  implicit def deriveInstance[T, LKV]
    (implicit
      lgen: LabelledGeneric.Aux[T, LKV],
      lwclkv: Lazy[Wrap[LKV]]
    ): C[T] = {
      import lwclkv.value._
      val to: T => V = (t: T) => unlabel(lgen.to(t))
      val from: V => T = (v: V) => lgen.from(label(v))
      typeClass.project(unwrap, to, from)
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

  implicit def deriveCCons[H, T <: Coproduct] (implicit ch: Lazy[C[H]], ct: Lazy[C[T]]): C[H :+: T] =
    typeClass.coproduct(ch.value, ct.value)
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
    new Wrap[CNil] {
      type V = CNil
      val unwrap = typeClass.emptyCoproduct
      def label(v: CNil): CNil = ???
      def unlabel(rec: CNil): CNil = ???
    }

  implicit def deriveCCons[HK <: Symbol, HV, TKV <: Coproduct]
    (implicit
      ch: Lazy[C[HV]],
      key: Witness.Aux[HK],
      ct: Lazy[Wrap[TKV] { type V <: Coproduct }]
    ): Wrap.Aux[FieldType[HK, HV] :+: TKV, HV :+: ct.value.V] =
      new Wrap[FieldType[HK, HV] :+: TKV] {
        type V = HV :+: ct.value.V
        val unwrap = typeClass.coproduct(key.value.name, ch.value, ct.value.unwrap)
        def label(v: HV :+: ct.value.V): FieldType[HK, HV] :+: TKV =
          v match {
            case Inl(hv) => Inl(field[HK](hv))
            case Inr(tv) => Inr(ct.value.label(tv))
          }
        def unlabel(rec: FieldType[HK, HV] :+: TKV): HV :+: ct.value.V =
          rec match {
            case Inl(hkv) => Inl(hkv)
            case Inr(tkv) => Inr(ct.value.unlabel(tkv))
          }
      }
}
