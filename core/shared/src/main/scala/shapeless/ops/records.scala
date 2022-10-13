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

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

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

  class UnsafeSelector(i: Int) extends Selector[HList, Any] {
    type Out = Any
    def apply(l: HList): Any = HList.unsafeGet(l, i)
  }

  class SelectorMacros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    def applyImpl[L <: HList, K](implicit lTag: WeakTypeTag[L], kTag: WeakTypeTag[K]): Tree = {
      val lTpe = lTag.tpe.dealias
      val kTpe = kTag.tpe.dealias
      if(!(lTpe <:< hlistTpe))
        abort(s"$lTpe is not a record type")

      findField(lTpe, kTpe) match {
        case Some((v, i)) =>
          q"""
            new _root_.shapeless.ops.record.UnsafeSelector($i).
              asInstanceOf[_root_.shapeless.ops.record.Selector.Aux[$lTpe, $kTpe, $v]]
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

  class UnsafeUpdater(i: Int) extends Updater[HList, Any] {
    type Out = HList
    def apply(l: HList, f: Any): HList = HList.unsafeUpdateAppend(l, i, f)
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
        val i0 = lTpes.indexWhere(_ =:= fTpe)
        if(i0 < 0) (lTpes :+ fTpe, lTpes.length)
        else (lTpes.updated(i0, fTpe), i0)
      }
      val uTpe = mkHListTpe(uTpes)
      q"""
        new _root_.shapeless.ops.record.UnsafeUpdater($i)
          .asInstanceOf[_root_.shapeless.ops.record.Updater.Aux[$lTpe, $fTpe, $uTpe]]
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
   * Type class support record deep merging.
   *
   * @author Ievgen Garkusha
   */
  trait DeepMerger[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable { type Out <: HList }

  trait LowPriorityDeepMerger {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = DeepMerger[L, M] { type Out = Out0 }

    implicit def hlistMerger1[H, T <: HList, M <: HList]
      (implicit mt : DeepMerger[T, M]): Aux[H :: T, M, H :: mt.Out] =
        new DeepMerger[H :: T, M] {
          type Out = H :: mt.Out
          def apply(l: H :: T, m: M): Out = l.head :: mt(l.tail, m)
        }
  }

  trait LowPriorityDeepMerger0 extends LowPriorityDeepMerger{
    implicit def hlistMerger2[K, V, T <: HList, M <: HList, MR <: HList]
      (implicit
        rm: Remover.Aux[M, K, (V, MR)],
        mt: DeepMerger[T, MR]
      ): Aux[FieldType[K, V] :: T, M, FieldType[K, V] :: mt.Out] =
      new DeepMerger[FieldType[K, V] :: T, M] {
        type Out = FieldType[K, V] :: mt.Out
        def apply(l: FieldType[K, V] :: T, m: M): Out = {
          val (mv, mr) = rm(m)
          val up = field[K](mv)
          up :: mt(l.tail, mr)
        }
      }
  }

  object DeepMerger extends LowPriorityDeepMerger0 {

    def apply[L <: HList, M <: HList](implicit dm: DeepMerger[L, M]): Aux[L, M, dm.Out] = dm

    implicit def hnilMerger[M <: HList]: Aux[HNil, M, M] =
      new DeepMerger[HNil, M] {
        type Out = M
        def apply(l: HNil, m: M): Out = m
      }

    implicit def hlistMerger3[K, V <: HList, T <: HList, M <: HList, V1 <: HList, MT <: HList,  MO1 <: HList, MO2 <: HList]
      (implicit
        rm: Remover.Aux[M, K, (V1, MT)],
        m1: DeepMerger.Aux[V, V1, MO1],
        m2: DeepMerger.Aux[T, MT, MO2]
      ): Aux[FieldType[K, V] :: T, M, FieldType[K, MO1] :: MO2] =
      new DeepMerger[FieldType[K, V] :: T, M] {
        type Out = FieldType[K, MO1] :: MO2
        def apply(r1: FieldType[K, V] :: T, r2: M ): Out = {
          val (rh, rt) = rm(r2)
          field[K](m1(r1.head, rh)) :: m2(r1.tail, rt)
        }
      }
  }

  /**
    * Type class supporting extraction of super-record from sub-record (witnesses depth subtype relation).
    *
    * @author Ievgen Garkusha
    */
  trait Extractor[L <: HList, E <: HList] extends Function1[L, E] with Serializable

  trait LowPriorityExtractor {
    implicit def extract[L <: HList, K, V, ET <: HList, V1, LR <: HList]
    (implicit
      ev0: L =:!= (FieldType[K, V] :: ET),
      r: Remover.Aux[L, K, (V1, LR)],
      ev: V1 <:< V,
      ds: Extractor[LR, ET]
    ): Extractor[L, FieldType[K, V] :: ET] =
    new Extractor[L, FieldType[K, V] :: ET] {
      def apply(c: L): FieldType[K, V] :: ET = {
        val (h, t) = r(c)
        field[K](ev(h)) :: ds(t)
      }
    }
  }

  object Extractor extends LowPriorityExtractor {

    def apply[L <: HList, E <: HList](implicit extractor: Extractor[L, E]): Extractor[L, E] = extractor

    implicit def hnil[L <: HList, E <: HList](implicit ev: HNil =:= E): Extractor[L, E] = new Extractor[L, E] {
      def apply(c: L): E = HNil
    }

    private val identicalInst = new Extractor[HList, HList] {
      def apply(c: HList): HList = c
    }

    implicit def identical[L <: HList]: Extractor[L, L] = identicalInst.asInstanceOf[Extractor[L, L ]]

    implicit def descend[L <: HList, K, V <: HList, V1 <: HList, LR <: HList, ET <: HList]
    (implicit
      ev0: L =:!= (FieldType[K, V] :: ET),
      r: Remover.Aux[L, K, (V1, LR)],
      ds1: Extractor[V1, V],
      ds2: Extractor[LR, ET]
    ): Extractor[L, FieldType[K, V] :: ET] =
    new Extractor[L, FieldType[K, V] :: ET] {
      def apply(c: L): FieldType[K, V] :: ET = {
        val (h, t) = r(c)
        field[K](ds1(h)) :: ds2(t)
      }
    }
  }

  /**
    * Type class support record merging with a callback function.
    *
    * @author Yang Bo
    */
  trait MergeWith[L <: HList, M <: HList, F] extends DepFn2[L, M] with Serializable { type Out <: HList }

  trait LowPriorityMergeWith {
    type Aux[L <: HList, M <: HList, F, Out0 <: HList] = MergeWith[L, M, F] { type Out = Out0 }

    implicit def hlistMergeWith1[K, V, T <: HList, M <: HList, F]
    (implicit
      mt : MergeWith[T, M, F],
      lacksKey: LacksKey[M, K]
    ): Aux[FieldType[K, V] :: T, M, F, FieldType[K, V] :: mt.Out] =
      new MergeWith[FieldType[K, V] :: T, M, F] {
        type Out = FieldType[K, V] :: mt.Out
        def apply(l: FieldType[K, V] :: T, m: M): Out = l.head :: mt(l.tail, m)
      }
  }

  object MergeWith extends LowPriorityMergeWith {
    def apply[L <: HList, M <: HList, F](implicit mergeWith: MergeWith[L, M, F]): Aux[L, M, F, mergeWith.Out] = mergeWith

    implicit def hnilMergeWith[M <: HList, F]: Aux[HNil, M, F, M] =
      new MergeWith[HNil, M, F] {
        type Out = M
        def apply(l: HNil, m: M): Out = m
      }

    implicit def hlistMergeWith2[K, V0, V1, V, T <: HList, M <: HList, MT <: HList, F, Out0 <: HList]
    (implicit
      rm: Remover.Aux[M, K, (V1, MT)],
      mt: MergeWith.Aux[T, MT, F, Out0],
      callback: PolyDefns.Case2.Aux[F, V0, V1, V]
    ): Aux[FieldType[K, V0] :: T, M, F, FieldType[K, V] :: Out0] = {
      new MergeWith[FieldType[K, V0] :: T, M, F] {
        type Out = FieldType[K, V] :: mt.Out
        def apply(l: FieldType[K, V0] :: T, m: M): Out = {
          val (mv, mr) = rm(m)
          val up = field[K](callback(l.head: V0, mv))
          up :: mt(l.tail, mr)
        }
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

    implicit def mkModifier[L <: HList, K, A, B, O <: HList]: Aux[L, K, A, B, O] = macro ModifierMacros.applyImpl[L, K, A, B]

    @deprecated("Retained for binary compatability", "2.3.1")
    def hlistModify1[F, A, B, T <: HList]: Aux[FieldType[F, A] :: T, F, A, B, FieldType[F, B] :: T] =
      new Modifier[FieldType[F, A] :: T, F, A, B] {
        type Out = FieldType[F, B] :: T
        def apply(l: FieldType[F, A] :: T, f: A => B): Out = field[F](f(l.head)) :: l.tail
      }

    @deprecated("Retained for binary compatability", "2.3.1")
    def hlistModify[H, T <: HList, F, A, B]
      (implicit mt: Modifier[T, F, A, B]): Aux[H :: T, F, A, B, H :: mt.Out] =
        new Modifier[H :: T, F, A, B] {
          type Out = H :: mt.Out
          def apply(l: H :: T, f: A => B): Out = l.head :: mt(l.tail, f)
        }
  }

  class UnsafeModifier(i: Int) extends Modifier[HList, Any, Any, Any] {
    type Out = HList
    def apply(l: HList, f: Any => Any): HList = HList.unsafeUpdateWith(l, i, f)
  }

  class ModifierMacros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    def applyImpl[L <: HList, K, A, B]
      (implicit lTag: WeakTypeTag[L], kTag: WeakTypeTag[K], aTag: WeakTypeTag[A], bTag: WeakTypeTag[B]): Tree = {
      val lTpe = lTag.tpe.dealias
      val kTpe = kTag.tpe.dealias
      if(!(lTpe <:< hlistTpe))
        abort(s"$lTpe is not a record type")
      val aTpe = aTag.tpe
      val bTpe = bTag.tpe

      findField(lTpe, kTpe) match {
        case Some((v, i)) if v <:< aTpe =>
          val (prefix, List(_, suffix @ _*)) = (unpackHListTpe(lTpe).splitAt(i): @unchecked)
          val outTpe = mkHListTpe(prefix ++ (FieldType(kTpe, bTpe) :: suffix.toList))
          q"""
            new _root_.shapeless.ops.record.UnsafeModifier($i).
              asInstanceOf[_root_.shapeless.ops.record.Modifier.Aux[$lTpe, $kTpe, $aTpe, $bTpe, $outTpe]]
          """
        case _ =>
          abort(s"No field $kTpe in record type $lTpe")
      }
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

    @deprecated("Retained for binary compatability", "2.3.1")
    def hlistRemove[H, T <: HList, K, V, OutT <: HList]
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

    implicit def mkRemover[L <: HList, K, V, O <: HList]: Aux[L, K, (V, O)] = macro RemoverMacros.applyImpl[L, K]

    @deprecated("Retained for binary compatability", "2.3.1")
    def hlistRemove1[K, V, T <: HList]: Aux[FieldType[K, V] :: T, K, (V, T)] =
      new Remover[FieldType[K, V] :: T, K] {
        type Out = (V, T)
        def apply(l: FieldType[K, V] :: T): Out = (l.head, l.tail)
      }
  }

  class UnsafeRemover(i: Int) extends Remover[HList, Any] {
    type Out = (Any, HList)
    def apply(l: HList): (Any, HList) = HList.unsafeRemove(l, i)
  }

  class RemoverMacros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    def applyImpl[L <: HList, K](implicit lTag: WeakTypeTag[L], kTag: WeakTypeTag[K]): Tree = {
      val lTpe = lTag.tpe.dealias
      val kTpe = kTag.tpe.dealias
      if(!(lTpe <:< hlistTpe))
        abort(s"$lTpe is not a record type")

      findField(lTpe, kTpe) match {
        case Some((v, i)) =>
          val (prefix, List(_, suffix @ _*)) = (unpackHListTpe(lTpe).splitAt(i): @unchecked)
          val outTpe = mkHListTpe(prefix ++ suffix)
          q"""
            new _root_.shapeless.ops.record.UnsafeRemover($i).
              asInstanceOf[_root_.shapeless.ops.record.Remover.Aux[$lTpe, $kTpe, ($v, $outTpe)]]
          """
        case _ =>
          abort(s"No field $kTpe in record type $lTpe")
      }
    }
  }

  /**
   * Type class supporting removal and re-insertion of an element (possibly unlabelled).
   *
   * @author Travis Brown
   */
  @annotation.implicitNotFound(msg = "No field or element type ${E} in record ${L}")
  trait Remove[L, E] extends DepFn1[L] with Serializable {
    def reinsert(out: Out): L
  }

  trait LowPriorityRemove {
    type Aux[L <: HList, E, Out0] = Remove[L, E] { type Out = Out0 }

    implicit def hconsRemove[H, T <: HList, E, OutT <: HList]
      (implicit rt: Aux[T, E, (E, OutT)]): Aux[H :: T, E, (E, H :: OutT)] =
        new Remove[H :: T, E] {
          type Out = (E, H :: OutT)

          def apply(l: H :: T): Out = {
            val (e, tail) = rt(l.tail)
            (e, l.head :: tail)
          }

          def reinsert(out: Out): H :: T = out._2.head :: rt.reinsert((out._1, out._2.tail))
        }
  }

  object Remove extends LowPriorityRemove {
    def apply[L <: HList, E](implicit remove: Remove[L, E]): Aux[L, E, remove.Out] = remove

    implicit def removeHead[K, V, T <: HList]: Aux[FieldType[K, V] :: T, FieldType[K, V], (FieldType[K, V], T)] =
      new Remove[FieldType[K, V] :: T, FieldType[K, V]] {
        type Out = (FieldType[K, V], T)

        def apply(l: FieldType[K, V] :: T): Out = (l.head, l.tail)
        def reinsert(out: Out): FieldType[K, V] :: T = out._1 :: out._2
      }

    implicit def removeUnlabelledHead[K, V, T <: HList]: Aux[FieldType[K, V] :: T, V, (V, T)] =
      new Remove[FieldType[K, V] :: T, V] {
        type Out = (V, T)

        def apply(l: FieldType[K, V] :: T): Out = (l.head, l.tail)
        def reinsert(out: Out): FieldType[K, V] :: T = field[K](out._1) :: out._2
      }
  }

  /**
   * Type class supporting removal and re-insertion of an `HList` of elements (possibly unlabelled).
   *
   * @author Travis Brown
   */
  @annotation.implicitNotFound(msg = "Not all of the field or element types ${A} are in record ${L}")
  trait RemoveAll[L <: HList, A <: HList] extends DepFn1[L] with Serializable {
    def reinsert(out: Out): L
  }

  object RemoveAll {
    type Aux[L <: HList, A <: HList, Out0] = RemoveAll[L, A] { type Out = Out0 }

    def apply[L <: HList, A <: HList](implicit removeAll: RemoveAll[L, A]): Aux[L, A, removeAll.Out] = removeAll

    implicit def hnilRemoveAll[L <: HList]: Aux[L, HNil, (HNil, L)] =
      new RemoveAll[L, HNil] {
        type Out = (HNil, L)

        def apply(l: L): Out = (HNil, l)
        def reinsert(out: Out): L = out._2
      }

    implicit def hconsRemoveAll[L <: HList, H, T <: HList, OutT <: HList, RemovedH, RemainderH <: HList, RemovedT <: HList, RemainderT <: HList]
      (implicit
        rt: RemoveAll.Aux[L, T, (RemovedT, RemainderT)],
        rh: Remove.Aux[RemainderT, H, (RemovedH, RemainderH)]
      ): Aux[L, H :: T, (RemovedH :: RemovedT, RemainderH)] =
        new RemoveAll[L, H :: T] {
          type Out = (RemovedH :: RemovedT, RemainderH)

          def apply(l: L): Out = {
            val (removedT, remainderT) = rt(l)
            val (removedH, remainderH) = rh(remainderT)
            (removedH :: removedT, remainderH)
          }

          def reinsert(out: Out): L = rt.reinsert((out._1.tail, rh.reinsert((out._1.head, out._2))))
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

  trait LacksKey[L <: HList, K]

  object LacksKey {
    implicit def apply[L <: HList, K]: LacksKey[L, K] = macro LacksKeyMacros.applyImpl[L, K]
  }

  class LacksKeyMacros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    def applyImpl[L <: HList, K](implicit lTag: WeakTypeTag[L], kTag: WeakTypeTag[K]): Tree = {
      val lTpe = lTag.tpe.dealias
      val kTpe = kTag.tpe.dealias
      if(!(lTpe <:< hlistTpe))
        abort(s"$lTpe is not a record type")

      findField(lTpe, kTpe) match {
        case None =>
          q"""
            new _root_.shapeless.ops.record.LacksKey[$lTpe, $kTpe] {}
          """
        case _ =>
          abort(s"Record type $lTpe contains field $kTpe")
      }
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
    * Type class supporting collecting the keys tagged by value types as a `HList` of `FieldType[V, K]`.
    *
    * @author Kailuo Wang
    */
  trait SwapRecord[L <: HList] extends DepFn0 with Serializable { type Out <: HList }

  object SwapRecord {
    def apply[L <: HList](implicit sr: SwapRecord[L]): Aux[L, sr.Out] = sr

    type Aux[L <: HList, Out0 <: HList] = SwapRecord[L] { type Out = Out0 }

    implicit def hnilSwapRecord[L <: HNil]: Aux[L, HNil] =
      new SwapRecord[L] {
        type Out = HNil
        def apply(): Out = HNil
      }

    implicit def hlistSwapRecord[K, V, T <: HList](implicit wk: Witness.Aux[K], kt: SwapRecord[T]): Aux[FieldType[K, V] :: T, FieldType[V, K] :: kt.Out] =
      new SwapRecord[FieldType[K, V] :: T] {
        type Out = FieldType[V, K] :: kt.Out
        def apply(): Out = field[V](wk.value) :: kt()
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
   * Type class combining `Keys` and `Values` for convenience and compilation speed.
   * It's similar to `Fields`, but produces distinct `HList`s instead of a zipped one.
   *
   * @author Jisoo Park
   */
  trait UnzipFields[L <: HList] extends Serializable {
    type Keys <: HList
    type Values <: HList

    def keys: Keys
    def values(l: L): Values
  }

  object UnzipFields {
    def apply[L <: HList](implicit uf: UnzipFields[L]): Aux[L, uf.Keys, uf.Values] = uf

    type Aux[L <: HList, K <: HList, V <: HList] = UnzipFields[L] { type Keys = K; type Values = V }

    implicit def hnilUnzipFields[L <: HNil]: Aux[L, HNil, L] =
      new UnzipFields[L] {
        type Keys = HNil
        type Values = L
        def keys = HNil
        def values(l: L): L = l
      }

    implicit def hconsUnzipFields[K, V, T <: HList](implicit
      key: Witness.Aux[K],
      tailUF: UnzipFields[T]
    ): Aux[FieldType[K, V] :: T, K :: tailUF.Keys, V :: tailUF.Values] =
      new UnzipFields[FieldType[K, V] :: T] {
        type Keys = K :: tailUF.Keys
        type Values = V :: tailUF.Values

        def keys = key.value :: tailUF.keys
        def values(l: FieldType[K, V] :: T) = l.head :: tailUF.values(l.tail)
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

  /**
    * Type class reordering record T by the HList of Keys K
    *
    * @author Mike Limansky
    */
  trait AlignByKeys[T <: HList, K <: HList] extends DepFn1[T] with Serializable {
    override type Out <: HList
  }

  object AlignByKeys {
    type Aux[T <: HList, K <: HList, O] = AlignByKeys[T, K] { type Out = O}

    def apply[T <: HList, K <: HList](implicit ev: AlignByKeys[T, K]): Aux[T, K, ev.Out] = ev

    implicit val hnilAlign: Aux[HNil, HNil, HNil] = new AlignByKeys[HNil, HNil] {
      override type Out = HNil
      override def apply(t: HNil): HNil = HNil
    }

    implicit def hlistAlign[T <: HList, KH, KT <: HList, V, R <: HList, TA <: HList](implicit
      remover: Remover.Aux[T, KH, (V, R)],
      tailAlign: AlignByKeys.Aux[R, KT, TA]
    ): Aux[T, KH :: KT, FieldType[KH, V] :: TA] = new AlignByKeys[T, KH :: KT] {
      override type Out = FieldType[KH, V] :: TA

      override def apply(t: T): FieldType[KH, V] :: TA = {
        val (v, r) = remover(t)
        field[KH](v) :: tailAlign(r)
      }
    }
  }
}
