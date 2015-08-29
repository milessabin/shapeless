/*
 * Copyright (c) 2011-15 Miles Sabin
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

import scala.language.experimental.macros
import scala.reflect.macros.{ blackbox, whitebox }

import poly._

//object record {
//  Ideally this would be an object rather than a package, however that appears
//  to trip bugs in implicit resolution which manifest in the use of WitnessWith
//  in updateWith
package record {
  import shapeless.labelled._

  /**
   * Type class supporting record field selection.
   *
   * @author Miles Sabin
   */
  @annotation.implicitNotFound(msg = "No field ${K} in record ${L}")
  trait Selector[L <: HList, K] extends DepFn1[L] with Serializable {
    type Out
    def apply(l : L): Out
  }

  object Selector {
    type Aux[L <: HList, K, Out0] = Selector[L, K] { type Out = Out0 }

    def apply[L <: HList, K](implicit selector: Selector[L, K]): Aux[L, K, selector.Out] = selector

    implicit def mkSelector[L <: HList, K, O]: Aux[L, K, O] = macro SelectorMacros.applyImpl[L, K]
  }

  class SelectorMacros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    def applyImpl[L <: HList, K](implicit lTag: WeakTypeTag[L], kTag: WeakTypeTag[K]): Tree = {
      val lTpe = lTag.tpe.dealias
      val kTpe = kTag.tpe.dealias
      if(!(lTpe <:< hlistTpe))
        abort(s"$lTpe is not a record type")

      val lTpes = unpackHListTpe(lTpe).zipWithIndex.flatMap { case (fTpe, i) =>
        val (k, v) = unpackFieldType(fTpe)
        if(k.dealias =:= kTpe) Some((v, i)) else None
      }
      lTpes.headOption match {
        case Some((vTpe, i)) =>
          q"""
            new _root_.shapeless.ops.record.Selector[$lTpe, $kTpe] {
              type Out = $vTpe
              def apply(l: $lTpe): $vTpe = _root_.shapeless.HList.unsafeGet(l, $i).asInstanceOf[$vTpe]
            }: _root_.shapeless.ops.record.Selector.Aux[$lTpe, $kTpe, $vTpe]
          """
        case _ =>
          abort(s"No field $kTpe in record type $lTpe")
      }
    }
  }

  /**
   * Type class supporting multiple record field selection.
   *
   * @author Miles Sabin
   */
  @annotation.implicitNotFound(msg = "No fields ${K} in record ${L}")
  trait SelectAll[L <: HList, K <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object SelectAll {
    def apply[L <: HList, K <: HList](implicit sa: SelectAll[L, K]): Aux[L, K, sa.Out] = sa

    type Aux[L <: HList, K <: HList, Out0 <: HList] = SelectAll[L, K] { type Out = Out0 }

    implicit def hnilSelectAll[L <: HList]: Aux[L, HNil, HNil] =
      new SelectAll[L, HNil] {
        type Out = HNil
        def apply(l: L): Out = HNil
      }

    implicit def hconsSelectAll[L <: HList, KH, KT <: HList]
      (implicit
        sh: Selector[L, KH],
        st: SelectAll[L, KT]
      ): Aux[L, KH :: KT, sh.Out :: st.Out] =
      new SelectAll[L, KH :: KT] {
        type Out = sh.Out :: st.Out
        def apply(l: L): Out = sh(l) :: st(l)
      }
  }

  /**
   * Type class supporting record update and extension.
   *
   * @author Miles Sabin
   */
  trait Updater[L <: HList, F] extends DepFn2[L, F] with Serializable { type Out <: HList }

  object Updater {
    type Aux[L <: HList, F, Out0 <: HList] = Updater[L, F] { type Out = Out0 }

    def apply[L <: HList, F](implicit updater: Updater[L, F]): Aux[L, F, updater.Out] = updater

    implicit def mkUpdater[L <: HList, F, O]: Aux[L, F, O] = macro UpdaterMacros.applyImpl[L, F]
  }

  class UpdaterMacros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    def applyImpl[L <: HList, F](implicit lTag: WeakTypeTag[L], fTag: WeakTypeTag[F]): Tree = {
      val lTpe = lTag.tpe.dealias
      val fTpe = fTag.tpe.dealias
      if(!(lTpe <:< hlistTpe))
        abort(s"$lTpe is not a record type")

      val lTpes = unpackHListTpe(lTpe)
      val (uTpes, i) = {
        val i0 = lTpes.indexWhere(_.dealias =:= fTpe)
        if(i0 < 0) (lTpes :+ fTpe, lTpes.length)
        else (lTpes.updated(i0, fTpe), i0)
      }
      val uTpe = mkHListTpe(uTpes)
      q"""
        new _root_.shapeless.ops.record.Updater[$lTpe, $fTpe] {
          type Out = $uTpe
          def apply(l: $lTpe, f: $fTpe): $uTpe =
            _root_.shapeless.HList.unsafeUpdate(l, $i, f).asInstanceOf[$uTpe]
        }: _root_.shapeless.ops.record.Updater.Aux[$lTpe, $fTpe, $uTpe]
      """
    }
  }

  /**
   * Type class support record merging.
   *
   * @author Miles Sabin
   */
  trait Merger[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable { type Out <: HList }

  trait LowPriorityMerger {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = Merger[L, M] { type Out = Out0 }

    implicit def hlistMerger1[H, T <: HList, M <: HList]
      (implicit mt : Merger[T, M]): Aux[H :: T, M, H :: mt.Out] =
        new Merger[H :: T, M] {
          type Out = H :: mt.Out
          def apply(l: H :: T, m: M): Out = l.head :: mt(l.tail, m)
        }
  }

  object Merger extends LowPriorityMerger {
    def apply[L <: HList, M <: HList](implicit merger: Merger[L, M]): Aux[L, M, merger.Out] = merger

    implicit def hnilMerger[M <: HList]: Aux[HNil, M, M] =
      new Merger[HNil, M] {
        type Out = M
        def apply(l: HNil, m: M): Out = m
      }

    implicit def hlistMerger2[K, V, T <: HList, M <: HList, MT <: HList]
      (implicit
        rm: Remover.Aux[M, K, (V, MT)],
        mt: Merger[T, MT]
      ): Aux[FieldType[K, V] :: T, M, FieldType[K, V] :: mt.Out] =
      new Merger[FieldType[K, V] :: T, M] {
        type Out = FieldType[K, V] :: mt.Out
        def apply(l: FieldType[K, V] :: T, m: M): Out = {
          val (mv, mr) = rm(m)
          val up = field[K](mv)
          up :: mt(l.tail, mr)
        }
      }
  }

  /**
   * Type class supporting modification of a record field by given function.
   *
   * @author Joni Freeman
   */
  @annotation.implicitNotFound(msg = "No field ${F} with value of type ${A} in record ${L}")
  trait Modifier[L <: HList, F, A, B] extends DepFn2[L, A => B] with Serializable { type Out <: HList }

  object Modifier {
    def apply[L <: HList, F, A, B](implicit modifier: Modifier[L, F, A, B]): Aux[L, F, A, B, modifier.Out] = modifier

    type Aux[L <: HList, F, A, B, Out0 <: HList] = Modifier[L, F, A, B] { type Out = Out0 }

    implicit def hlistModify1[F, A, B, T <: HList]: Aux[FieldType[F, A] :: T, F, A, B, FieldType[F, B] :: T] =
      new Modifier[FieldType[F, A] :: T, F, A, B] {
        type Out = FieldType[F, B] :: T
        def apply(l: FieldType[F, A] :: T, f: A => B): Out = field[F](f(l.head)) :: l.tail
      }

    implicit def hlistModify[H, T <: HList, F, A, B]
      (implicit mt: Modifier[T, F, A, B]): Aux[H :: T, F, A, B, H :: mt.Out] =
        new Modifier[H :: T, F, A, B] {
          type Out = H :: mt.Out
          def apply(l: H :: T, f: A => B): Out = l.head :: mt(l.tail, f)
        }
  }

  /**
   * Type class supporting record field removal.
   *
   * @author Miles Sabin
   */
  @annotation.implicitNotFound(msg = "No field ${K} in record ${L}")
  trait Remover[L <: HList, K] extends DepFn1[L] with Serializable

  trait LowPriorityRemover {
    type Aux[L <: HList, K, Out0] = Remover[L, K] { type Out = Out0 }

    implicit def hlistRemove[H, T <: HList, K, V, OutT <: HList]
      (implicit rt: Aux[T, K, (V, OutT)]): Aux[H :: T, K, (V, H :: OutT)] =
        new Remover[H :: T, K] {
          type Out = (V, H :: OutT)
          def apply(l : H :: T): Out = {
            val (v, tail) = rt(l.tail)
            (v, l.head :: tail)
          }
        }
  }

  object Remover extends LowPriorityRemover {
    def apply[L <: HList, K](implicit remover: Remover[L, K]): Aux[L, K, remover.Out] = remover

    implicit def hlistRemove1[K, V, T <: HList]: Aux[FieldType[K, V] :: T, K, (V, T)] =
      new Remover[FieldType[K, V] :: T, K] {
        type Out = (V, T)
        def apply(l: FieldType[K, V] :: T): Out = (l.head, l.tail)
      }
  }

  /**
   * Type class supporting renaming of a record field.
   *
   * @author Joni Freeman
   */
  @annotation.implicitNotFound(msg = "No field ${K1} in record ${L}")
  trait Renamer[L <: HList, K1, K2] extends DepFn1[L] with Serializable { type Out <: HList }

  object Renamer {
    def apply[L <: HList, K1, K2](implicit renamer: Renamer[L, K1, K2]): Aux[L, K1, K2, renamer.Out] = renamer

    type Aux[L <: HList, K1, K2, Out0 <: HList] = Renamer[L, K1, K2] { type Out = Out0 }

    implicit def hlistRenamer1[T <: HList, K1, K2, V]: Aux[FieldType[K1, V] :: T, K1, K2, FieldType[K2, V] :: T] =
      new Renamer[FieldType[K1, V] :: T, K1, K2] {
        type Out = FieldType[K2, V] :: T
        def apply(l: FieldType[K1, V] :: T): Out = field[K2](l.head : V) :: l.tail
      }

    implicit def hlistRenamer[H, T <: HList, K1, K2, V]
      (implicit rn: Renamer[T, K1, K2]): Aux[H :: T, K1, K2, H :: rn.Out] =
        new Renamer[H :: T, K1, K2] {
          type Out = H :: rn.Out
          def apply(l: H :: T): Out = l.head :: rn(l.tail)
        }
  }

  /**
   * Type class supporting collecting the keys of a record as an `HList`.
   *
   * @author Miles Sabin
   */
  trait Keys[L <: HList] extends DepFn0 with Serializable { type Out <: HList }

  object Keys {
    def apply[L <: HList](implicit keys: Keys[L]): Aux[L, keys.Out] = keys

    type Aux[L <: HList, Out0 <: HList] = Keys[L] { type Out = Out0 }

    implicit def hnilKeys[L <: HNil]: Aux[L, HNil] =
      new Keys[L] {
        type Out = HNil
        def apply(): Out = HNil
      }

    implicit def hlistKeys[K, V, T <: HList](implicit wk: Witness.Aux[K], kt: Keys[T]): Aux[FieldType[K, V] :: T, K :: kt.Out] =
      new Keys[FieldType[K, V] :: T] {
        type Out = K :: kt.Out
        def apply(): Out = wk.value :: kt()
      }
  }

  /**
   * Type class supporting collecting the value of a record as an `HList`.
   *
   * @author Miles Sabin
   */
  trait Values[L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object Values {
    def apply[L <: HList](implicit values: Values[L]): Aux[L, values.Out] = values

    type Aux[L <: HList, Out0 <: HList] = Values[L] { type Out = Out0 }

    implicit def hnilValues[L <: HNil]: Aux[L, HNil] =
      new Values[L] {
        type Out = HNil
        def apply(l: L): Out = HNil
      }

    implicit def hlistValues[K, V, T <: HList](implicit vt: Values[T]): Aux[FieldType[K, V] :: T, V :: vt.Out] =
      new Values[FieldType[K, V] :: T] {
        type Out = V :: vt.Out
        def apply(l: FieldType[K, V] :: T): Out = (l.head: V) :: vt(l.tail)
      }
  }

  /**
   * Type class supporting converting this record to a `HList` of key-value pairs.
   *
   * @author Alexandre Archambault
   */
  trait Fields[L <: HList] extends DepFn1[L] with Serializable {
    type Out <: HList
  }

  object Fields {
    def apply[L <: HList](implicit fields: Fields[L]): Aux[L, fields.Out] = fields

    type Aux[L <: HList, Out0 <: HList] = Fields[L] { type Out = Out0 }

    implicit def hnilFields[L <: HNil]: Aux[L, L] =
      new Fields[L] {
        type Out = L
        def apply(l: L) = l
      }

    implicit def hconsFields[K, V, T <: HList](implicit
      key: Witness.Aux[K],
      tailFields: Fields[T]
    ): Aux[FieldType[K, V] :: T, (K, V) :: tailFields.Out] =
      new Fields[FieldType[K, V] :: T] {
        type Out = (K, V) :: tailFields.Out
        def apply(l: FieldType[K, V] :: T) = (key.value -> l.head) :: tailFields(l.tail)
      }
  }

  /**
   * Type class supporting converting this record to a `Map` whose keys and values
   * are typed as the Lub of the keys and values of this record.
   *
   * @author Alexandre Archambault
   */
  trait ToMap[L <: HList] extends DepFn1[L] with Serializable {
    type Key
    type Value
    type Out = Map[Key, Value]
  }

  object ToMap {
    def apply[L <: HList](implicit toMap: ToMap[L]): Aux[L, toMap.Key, toMap.Value] = toMap

    type Aux[L <: HList, Key0, Value0] = ToMap[L] { type Key = Key0; type Value = Value0 }

    implicit def hnilToMap[K, V, L <: HNil]: Aux[L, K, V] =
      new ToMap[L] {
        type Key = K
        type Value = V
        def apply(l: L) = Map.empty
      }

    implicit def hnilToMapAnyNothing[L <: HNil]: Aux[L, Any, Nothing] = hnilToMap[Any, Nothing, L]

    implicit def hsingleToMap[K, V](implicit
      wk: Witness.Aux[K]
    ): Aux[FieldType[K, V] :: HNil, K, V] =
      new ToMap[FieldType[K, V] :: HNil] {
        type Key = K
        type Value = V
        def apply(l: FieldType[K, V] :: HNil) = Map(wk.value -> (l.head: V))
      }

    implicit def hlistToMap[HK, HV, TH, TT <: HList, TK, TV, K, V](implicit
      tailToMap: ToMap.Aux[TH :: TT, TK, TV],
      keyLub: Lub[HK, TK, K],
      valueLub: Lub[HV, TV, V],
      wk: Witness.Aux[HK]
    ): Aux[FieldType[HK, HV] :: TH :: TT, K, V] =
      new ToMap[FieldType[HK, HV] :: TH :: TT] {
        type Key = K
        type Value = V
        def apply(l: FieldType[HK, HV] :: TH :: TT) =
          tailToMap(l.tail).map{case (k, v) => keyLub.right(k) -> valueLub.right(v)} +
            (keyLub.left(wk.value) -> valueLub.left(l.head: HV))
      }
  }

  /**
   * Type class supporting mapping a higher rank function over the values of a record.
   *
   * @author Alexandre Archambault
   */
  trait MapValues[HF, L <: HList] extends DepFn1[L] with Serializable { type Out <: HList }

  object MapValues {
    def apply[HF, L <: HList](implicit mapValues: MapValues[HF, L]): Aux[HF, L, mapValues.Out] = mapValues

    type Aux[HF, L <: HList, Out0 <: HList] = MapValues[HF, L] { type Out = Out0 }

    implicit def hnilMapValues[HF, L <: HNil]: Aux[HF, L, HNil] =
      new MapValues[HF, L] {
        type Out = HNil
        def apply(l: L) = HNil
      }

    implicit def hconsMapValues[HF, K, V, T <: HList](implicit
      hc: Case1[HF, V],
      mapValuesTail: MapValues[HF, T]
    ): Aux[HF, FieldType[K, V] :: T, FieldType[K, hc.Result] :: mapValuesTail.Out] =
      new MapValues[HF, FieldType[K, V] :: T] {
        type Out = FieldType[K, hc.Result] :: mapValuesTail.Out
        def apply(l: FieldType[K, V] :: T) = field[K](hc(l.head: V)) :: mapValuesTail(l.tail)
      }
  }
}
