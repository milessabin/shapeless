/*
 * Copyright (c) 2013 Miles Sabin 
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

object tuple {
  import shapeless.ops.{ hlist => hl }

  /**
   * Type class witnessing that this tuple is composite and providing access to head and tail. 
   * 
   * @author Miles Sabin
   */
  trait IsComposite[P] {
    type H
    type T
      
    def head(p : P) : H
    def tail(p : P) : T
  }

  object IsComposite {
    type Aux[P, H0, T0] = IsComposite[P] { type H = H0; type T = T0 }
    implicit def isComposite[P, L <: HList, H0, T <: HList]
      (implicit gen: Generic.Aux[P, L], isHCons: hl.IsHCons.Aux[L, H0, T], tp: hl.Tupler[T]): Aux[P, H0, tp.Out] =
        new IsComposite[P] {
          type H = H0
          type T = tp.Out
          def head(p: P): H = isHCons.head(gen.to(p))
          def tail(p: P): T = tp(isHCons.tail(gen.to(p)))
        }
  }

  /**
   * Type class supporting prepending to this tuple.
   * 
   * @author Miles Sabin
   */
  trait Prepend[T, U] extends DepFn2[T, U]

  object Prepend {
    type Aux[T, U, Out0] = Prepend[T, U] { type Out = Out0 }
    implicit def prepend[T, L1 <: HList, U, L2 <: HList, L3 <: HList]
      (implicit gent: Generic.Aux[T, L1], genu: Generic.Aux[U, L2], prepend: hl.Prepend.Aux[L1, L2, L3], tp: hl.Tupler[L3]): Aux[T, U, tp.Out] =
        new Prepend[T, U] {
          type Out = tp.Out
          def apply(t: T, u: U): Out = prepend(gent.to(t), genu.to(u)).tupled
        }
  }

  /**
   * Type class supporting reverse prepending to this tuple.
   * 
   * @author Miles Sabin
   */
  trait ReversePrepend[T, U] extends DepFn2[T, U]

  object ReversePrepend {
    type Aux[T, U, Out0] = ReversePrepend[T, U] { type Out = Out0 }
    implicit def prepend[T, L1 <: HList, U, L2 <: HList, L3 <: HList]
      (implicit gent: Generic.Aux[T, L1], genu: Generic.Aux[U, L2], prepend: hl.ReversePrepend.Aux[L1, L2, L3], tp: hl.Tupler[L3]): Aux[T, U, tp.Out] =
        new ReversePrepend[T, U] {
          type Out = tp.Out
          def apply(t: T, u: U): Out = prepend(gent.to(t), genu.to(u)).tupled
        }
  }

  /**
   * Type class supporting access to the ''nth'' element of this tuple. Available only if this tuple has at least
   * ''n'' elements. 
   * 
   * @author Miles Sabin
   */
  trait At[T, N <: Nat] extends DepFn1[T]

  object At {
    type Aux[T, N <: Nat, Out0] = At[T, N] { type Out = Out0 }
    implicit def at[T, L1 <: HList, N <: Nat]
      (implicit gen: Generic.Aux[T, L1], at: hl.At[L1, N]): Aux[T, N, at.Out] =
        new At[T, N] {
          type Out = at.Out
          def apply(t: T): Out = at(gen.to(t))
        }
  }

  /**
   * Type class supporting access to the last element of this tuple. Available only if this tuple has at least one
   * element.
   * 
   * @author Miles Sabin
   */
  trait Last[T] extends DepFn1[T]

  object Last {
    type Aux[T, Out0] = Last[T] { type Out = Out0 }
    implicit def last[T, L <: HList]
      (implicit gen: Generic.Aux[T, L], last: hl.Last[L]): Aux[T, last.Out] =
        new Last[T] {
          type Out = last.Out
          def apply(t: T): Out = gen.to(t).last
        }
  }

  /**
   * Type class supporting access to all but the last element of this tuple. Available only if this tuple has at
   * least one element.
   * 
   * @author Miles Sabin
   */
  trait Init[T] extends DepFn1[T]

  object Init {
    type Aux[T, Out0] = Init[T] { type Out = Out0 }
    implicit def init[T, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], init: hl.Init.Aux[L1, L2], tp: hl.Tupler[L2]): Aux[T, tp.Out] =
        new Init[T] {
          type Out = tp.Out
          def apply(t: T): Out = init(gen.to(t)).tupled
        }
  }

  /**
   * Type class supporting access to the first element of this tuple of type `U`. Available only if this tuple
   * contains an element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait Selector[T, U] extends DepFn1[T] { type Out = U }

  object Selector {
    type Aux[T, U] = Selector[T, U]
    implicit def select[T, L <: HList, U]
      (implicit gen: Generic.Aux[T, L], selector: hl.Selector[L, U]): Aux[T, U] =
        new Selector[T, U] {
          def apply(t: T): U = gen.to(t).select[U]
        }
  }

  /**
   * Type class supporting access to the all elements of this tuple of type `U`.
   * 
   * @author Miles Sabin
   */
  trait Filter[T, U] extends DepFn1[T]

  object Filter {
    type Aux[T, U, Out0] = Filter[T, U] { type Out = Out0 }
    implicit def filterTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], filter: hl.Filter.Aux[L1, U, L2], tp: hl.Tupler[L2]): Aux[T, U, tp.Out] = new Filter[T, U] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filter(gen.to(t)))
      }
  }

  /**
   * Type class supporting access to the all elements of this tuple of type different than `U`.
   * 
   * @author Miles Sabin
   */
  trait FilterNot[T, U] extends DepFn1[T]

  object FilterNot {
    type Aux[T, U, Out0] = FilterNot[T, U] { type Out = Out0 }
    implicit def filterNotTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], filterNot: hl.FilterNot.Aux[L1, U, L2], tp: hl.Tupler[L2]): Aux[T, U, tp.Out] = new FilterNot[T, U] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filterNot(gen.to(t)))
      }
  }

  /**
   * Type class supporting removal of an element from this tuple. Available only if this tuple contains an
   * element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait Remove[T, U] extends DepFn1[T]

  object Remove {
    type Aux[T, U, Out0] = Remove[T, U] { type Out = Out0 }
    implicit def removeTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], remove: hl.Remove.Aux[L1, U, (U, L2)], tp: hl.Tupler[L2]): Aux[T, U, (U, tp.Out)] = new Remove[T, U] {
        type Out = (U, tp.Out)
        def apply(t: T): Out = { val (u, rem) = remove(gen.to(t)) ; (u, tp(rem)) }
      }
  }

  /**
   * Type class supporting removal of a sublist from this tuple. Available only if this tuple contains a
   * sublist of type `SL`.
   *
   * The elements of `SL` do not have to be contiguous in this tuple.
   * 
   * @author Miles Sabin
   */
  trait RemoveAll[T, S] extends DepFn1[T]

  object RemoveAll {
    type Aux[T, S, Out0] = RemoveAll[T, S] { type Out = Out0 }
    implicit def removeAllTuple[T, ST, SL <: HList, L1 <: HList, L2 <: HList]
      (implicit gent: Generic.Aux[T, L1], gens: Generic.Aux[ST, SL],  removeAll: hl.RemoveAll.Aux[L1, SL, (SL, L2)], tp: hl.Tupler[L2]): Aux[T, ST, (ST, tp.Out)] =
        new RemoveAll[T, ST] {
          type Out = (ST, tp.Out)
          def apply(t: T): Out = { val (e, rem) = removeAll(gent.to(t)) ; (gens.from(e), tp(rem)) }
        }
  }

  /**
   * Type class supporting replacement of the first element of type U from this tuple with an element of type V.
   * Available only if this tuple contains an element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait Replacer[T, U, V] extends DepFn2[T, U]

  object Replacer {
    type Aux[T, U, V, Out0] = Replacer[T, U, V] { type Out = Out0 }
    implicit def replaceTuple[T, L1 <: HList, U, V, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], replace: hl.Replacer.Aux[L1, V, U, (V, L2)], tp: hl.Tupler[L2]): Aux[T, U, V, (V, tp.Out)] = new Replacer[T, U, V] {
        type Out = (V, tp.Out)
        def apply(t: T, u: U): Out = { val (v, rep) = replace(gen.to(t), u) ; (v, tp(rep)) }
      }
  }

  /**
   * Type class supporting replacement of the Nth element of this tuple with an element of type V. Available only if
   * this tuple contains at least N elements.
   * 
   * @author Miles Sabin
   */
  trait ReplaceAt[T, N <: Nat, U] extends DepFn2[T, U]

  object ReplaceAt {
    type Aux[T, N <: Nat, U, Out0] = ReplaceAt[T, N, U] { type Out = Out0 }
    implicit def replaceTuple[T, L1 <: HList, N <: Nat, U, V, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], replaceAt: hl.ReplaceAt.Aux[L1, N, U, (V, L2)], tp: hl.Tupler[L2]): Aux[T, N, U, (V, tp.Out)] = new ReplaceAt[T, N, U] {
        type Out = (V, tp.Out)
        def apply(t: T, u: U): Out = { val (v, rep) = replaceAt(gen.to(t), u) ; (v, tp(rep)) }
      }
  }

  /**
   * Type class supporting retrieval of the first ''n'' elements of this tuple. Available only if this tuple has at
   * least ''n'' elements.
   * 
   * @author Miles Sabin
   */
  trait Take[T, N <: Nat] extends DepFn1[T]

  object Take {
    type Aux[T, N <: Nat, Out0] = Take[T, N] { type Out = Out0 }
    implicit def tupleTake[T, L1 <: HList, N <: Nat, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], take: hl.Take.Aux[L1, N, L2], tp: hl.Tupler[L2]): Aux[T, N, tp.Out] = 
        new Take[T, N] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(take(gen.to(t)))
        }
  }

  /**
   * Type class supporting removal of the first ''n'' elements of this tuple. Available only if this tuple has at
   * least ''n'' elements.
   * 
   * @author Miles Sabin
   */
  trait Drop[T, N <: Nat] extends DepFn1[T]

  object Drop {
    type Aux[T, N <: Nat, Out0] = Drop[T, N] { type Out = Out0 }
    implicit def tupleDrop[T, L1 <: HList, N <: Nat, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], drop: hl.Drop.Aux[L1, N, L2], tp: hl.Tupler[L2]): Aux[T, N, tp.Out] =
        new Drop[T, N] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(drop(gen.to(t)))
        }
  }

  /**
   * Type class supporting splitting this tuple at the ''nth'' element returning the prefix and suffix as a pair.
   * Available only if this tuple has at least ''n'' elements.
   * 
   * @author Miles Sabin
   */
  trait Split[T, N <: Nat] extends DepFn1[T]

  object Split {
    type Aux[T, N <: Nat, Out0] = Split[T, N] { type Out = Out0 }
    implicit def tupleSplit[T, L <: HList, N <: Nat, LP <: HList, LS <: HList]
      (implicit gen: Generic.Aux[T, L], split: hl.Split.Aux[L, N, (LP, LS)], tpp: hl.Tupler[LP], tps: hl.Tupler[LS]): Aux[T, N, (tpp.Out, tps.Out)] =
        new Split[T, N] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the ''nth'' element returning the reverse prefix and suffix as a
   * pair. Available only if this tuple has at least ''n'' elements.
   * 
   * @author Miles Sabin
   */
  trait ReverseSplit[T, N <: Nat] extends DepFn1[T]

  object ReverseSplit {
    type Aux[T, N <: Nat, Out0] = ReverseSplit[T, N] { type Out = Out0 }
    implicit def tupleReverseSplit[T, L <: HList, N <: Nat, LP <: HList, LS <: HList]
      (implicit gen: Generic.Aux[T, L], split: hl.ReverseSplit.Aux[L, N, (LP, LS)], tpp: hl.Tupler[LP], tps: hl.Tupler[LS]): Aux[T, N, (tpp.Out, tps.Out)] =
        new ReverseSplit[T, N] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the first occurence of an element of type `U` returning the prefix
   * and suffix as a pair. Available only if this tuple contains an element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait SplitLeft[T, U] extends DepFn1[T]

  object SplitLeft {
    type Aux[T, U, Out0] = SplitLeft[T, U] { type Out = Out0 }
    implicit def tupleSplitLeft[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: Generic.Aux[T, L], split: hl.SplitLeft.Aux[L, U, (LP, LS)], tpp: hl.Tupler[LP], tps: hl.Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new SplitLeft[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the first occurence of an element of type `U` returning the reverse
   * prefix and suffix as a pair. Available only if this tuple contains an element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait ReverseSplitLeft[T, U] extends DepFn1[T]

  object ReverseSplitLeft {
    type Aux[T, U, Out0] = ReverseSplitLeft[T, U] { type Out = Out0 }
    implicit def tupleReverseSplitLeft[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: Generic.Aux[T, L], split: hl.ReverseSplitLeft.Aux[L, U, (LP, LS)], tpp: hl.Tupler[LP], tps: hl.Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new ReverseSplitLeft[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the last occurence of an element of type `U` returning the prefix
   * and suffix as a pair. Available only if this tuple contains an element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait SplitRight[T, U] extends DepFn1[T]

  object SplitRight {
    type Aux[T, U, Out0] = SplitRight[T, U] { type Out = Out0 }
    implicit def tupleSplitRight[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: Generic.Aux[T, L], split: hl.SplitRight.Aux[L, U, (LP, LS)], tpp: hl.Tupler[LP], tps: hl.Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new SplitRight[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the last occurence of an element of type `U` returning the reverse
   * prefix and suffix as a pair. Available only if this tuple contains an element of type `U`.
   * 
   * @author Miles Sabin
   */
  trait ReverseSplitRight[T, U] extends DepFn1[T]

  object ReverseSplitRight {
    type Aux[T, U, Out0] = ReverseSplitRight[T, U] { type Out = Out0 }
    implicit def tupleReverseSplitRight[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: Generic.Aux[T, L], split: hl.ReverseSplitRight.Aux[L, U, (LP, LS)], tpp: hl.Tupler[LP], tps: hl.Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new ReverseSplitRight[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting reversing this tuple.
   * 
   * @author Miles Sabin
   */
  trait Reverse[T] extends DepFn1[T]

  object Reverse {
    type Aux[T, Out0] = Reverse[T] { type Out = Out0 }
    implicit def tupleReverseAux[T, L1 <: HList, L2 <: HList, Out]
      (implicit gen: Generic.Aux[T, L1], reverse: hl.Reverse.Aux[L1, L2], tp: hl.Tupler[L2]): Aux[T, tp.Out] =
        new Reverse[T] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(reverse(gen.to(t)))
        }
  }

  /**
   * Type class supporting mapping a higher ranked function over this tuple. 
   * 
   * @author Miles Sabin
   */
  trait Mapper[T, P] extends DepFn1[T]

  object Mapper {
    type Aux[T, P, Out0] = Mapper[T, P] { type Out = Out0 }
    implicit def mapper[T, P, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], mapper: hl.Mapper.Aux[P, L1, L2], tp: hl.Tupler[L2]): Aux[T, P, tp.Out] =
        new Mapper[T, P] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(mapper(gen.to(t)))
        }
  }

  /**
   * Type class supporting flatmapping a higher ranked function over this tuple. 
   * 
   * @author Miles Sabin
   */
  trait FlatMapper[T, P] extends DepFn1[T]

  object FlatMapper {
    import poly.Compose
    type Aux[T, P, Out0] = FlatMapper[T, P] { type Out = Out0 }
    implicit def mapper[T, P, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], mapper: hl.FlatMapper.Aux[Compose[productElements.type, P], L1, L2], tp: hl.Tupler[L2]): Aux[T, P, tp.Out] =
        new FlatMapper[T, P] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(mapper(gen.to(t)))
        }
  }

  /**
   * Type class supporting mapping a constant valued function over this tuple. 
   * 
   * @author Miles Sabin
   */
  trait ConstMapper[T, C] extends DepFn2[T, C]

  object ConstMapper {
    type Aux[T, C, Out0] = ConstMapper[T, C] { type Out = Out0 }
    implicit def mapper[T, C, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], mapper: hl.ConstMapper.Aux[C, L1, L2], tp: hl.Tupler[L2]): Aux[T, C, tp.Out] =
        new ConstMapper[T, C] {
          type Out = tp.Out
          def apply(t: T, c: C): tp.Out = tp(mapper(c, gen.to(t)))
        }
  }

  /**
   * Type class supporting mapping a polymorphic function over this tuple and then folding the result using a
   * monomorphic function value. 
   * 
   * @author Miles Sabin
   */
  trait MapFolder[T, R, P] { // Nb. Not a dependent function signature
    def apply(t: T, in: R, op: (R, R) => R): R 
  }

  object MapFolder {
    implicit def mapper[T, L <: HList, R, P]
      (implicit gen: Generic.Aux[T, L], mapper: hl.MapFolder[L, R, P]): MapFolder[T, R, P] =
        new MapFolder[T, R, P] {
          def apply(t: T, in: R, op: (R, R) => R): R = mapper(gen.to(t), in, op)
        }
  }

  /**
   * Type class supporting left-folding a polymorphic binary function over this tuple.
   * 
   * @author Miles Sabin
   */
  trait LeftFolder[T, U, P] extends DepFn2[T, U]

  object LeftFolder {
    type Aux[T, U, P, Out0] = LeftFolder[T, U, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, U, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.LeftFolder[L, U, P]): Aux[T, U, P, folder.Out] =
        new LeftFolder[T, U, P] {
          type Out = folder.Out
          def apply(t: T, u: U): Out = folder(gen.to(t), u)
        }
  }

  /**
   * Type class supporting right-folding a polymorphic binary function over this tuple.
   * 
   * @author Miles Sabin
   */
  trait RightFolder[T, U, P] extends DepFn2[T, U]

  object RightFolder {
    type Aux[T, U, P, Out0] = RightFolder[T, U, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, U, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.RightFolder[L, U, P]): Aux[T, U, P, folder.Out] =
        new RightFolder[T, U, P] {
          type Out = folder.Out
          def apply(t: T, u: U): Out = folder(gen.to(t), u)
        }
  }
  
  /**
   * Type class supporting left-reducing a polymorphic binary function over this tuple.
   * 
   * @author Miles Sabin
   */
  trait LeftReducer[T, P] extends DepFn1[T]

  object LeftReducer {
    type Aux[T, P, Out0] = LeftReducer[T, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.LeftReducer[L, P]): Aux[T, P, folder.Out] =
        new LeftReducer[T, P] {
          type Out = folder.Out
          def apply(t: T): Out = folder(gen.to(t))
        }
  }
  
  /**
   * Type class supporting right-reducing a polymorphic binary function over this tuple.
   * 
   * @author Miles Sabin
   */
  trait RightReducer[T, P] extends DepFn1[T]

  object RightReducer {
    type Aux[T, P, Out0] = RightReducer[T, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.RightReducer[L, P]): Aux[T, P, folder.Out] =
        new RightReducer[T, P] {
          type Out = folder.Out
          def apply(t: T): Out = folder(gen.to(t))
        }
  }

  /**
   * Type class supporting transposing this tuple.
   * 
   * @author Miles Sabin
   */
  trait Transposer[T] extends DepFn1[T]

  object Transposer {
    type Aux[T, Out0] = Transposer[T] { type Out = Out0 }
    implicit def transpose[T, L1 <: HList, L2 <: HList, L3 <: HList, L4 <: HList]
      (implicit
        gen: Generic.Aux[T, L1],
        mpe: hl.Mapper.Aux[productElements.type, L1, L2],
        tps: hl.Transposer.Aux[L2, L3],
        mtp: hl.Mapper.Aux[tupled.type, L3, L4],
        tp:  hl.Tupler[L4]
      ): Aux[T, tp.Out] =
      new Transposer[T] {
        type Out = tp.Out
        def apply(t: T): Out = ((gen.to(t) map productElements).transpose map tupled).tupled
      }
  }

  /**
   * Type class supporting zipping this this tuple of monomorphic function values with its argument tuple of
   * correspondingly typed function arguments returning the result of each application as a tuple. Available only if
   * there is evidence that the corresponding function and argument elements have compatible types.
   * 
   * @author Miles Sabin
   */
  trait ZipApply[FT, AT] extends DepFn2[FT, AT]

  object ZipApply {
    type Aux[FT, AT, Out0] = ZipApply[FT, AT] { type Out = Out0 }
    implicit def zipApply[FT, FL <: HList, AT, AL <: HList, RL <: HList]
      (implicit
        genf: Generic.Aux[FT, FL],
        gena: Generic.Aux[AT, AL],
        zip:  hl.ZipApply.Aux[FL, AL, RL],
        tp:   hl.Tupler[RL]
      ): Aux[FT, AT, tp.Out] = 
      new ZipApply[FT, AT] {
        type Out = tp.Out
        def apply(ft: FT, at: AT): Out = (genf.to(ft) zipApply gena.to(at)).tupled
      }
  }

  /**
   * Type class supporting zipping this tuple with a tuple of tuples returning a tuple of tuples with each
   * element of this tuple prepended to the corresponding tuple element of the argument tuple.
   * 
   * @author Miles Sabin
   */
  trait ZipOne[H, T] extends DepFn2[H, T]

  object ZipOne {
    type Aux[H, T, Out0] = ZipOne[H, T] { type Out = Out0 }
    implicit def zipOne[HT, HL <: HList, TT, TL <: HList, TLL <: HList, RLL <: HList, RL <: HList]
      (implicit
        genh: Generic.Aux[HT, HL],
        gent: Generic.Aux[TT, TL],
        mpet: hl.Mapper.Aux[productElements.type, TL, TLL],
        zone: hl.ZipOne.Aux[HL, TLL, RLL],
        mtp:  hl.Mapper.Aux[tupled.type, RLL, RL],
        tp:   hl.Tupler[RL]
      ): Aux[HT, TT, tp.Out] =
      new ZipOne[HT, TT] {
        type Out = tp.Out
        def apply(h: HT, t: TT): Out = ((genh.to(h) zipOne (gent.to(t) map productElements)) map tupled).tupled
      }
  }

  /**
   * Type class supporting zipping a tuple with a constant, resulting in a tuple of tuples of the form
   * ({element from input tuple}, {supplied constant})
   *
   * @author Miles Sabin
   */
  trait ZipConst[T, C] extends DepFn2[T, C]

  object ZipConst {
    type Aux[T, C, Out0] = ZipConst[T, C] { type Out = Out0 }
    implicit def zipConst[T, C, L1 <: HList, L2 <: HList]
    (implicit gen: Generic.Aux[T, L1], zipper: hl.ZipConst.Aux[C, L1, L2], tp: hl.Tupler[L2]): Aux[T, C, tp.Out] =
      new ZipConst[T, C] {
        type Out = tp.Out
        def apply(t: T, c: C): tp.Out = tp(zipper(c, gen.to(t)))
      }
  }

  /**
   * Type class supporting unification of this tuple. 
   * 
   * @author Miles Sabin
   */
  trait Unifier[T] extends DepFn1[T]

  object Unifier {
    type Aux[T, Out0] = Unifier[T] { type Out = Out0 }
    implicit def unifier[T, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], unifier: hl.Unifier.Aux[L1, L2], tp: hl.Tupler[L2]): Aux[T, tp.Out] =
        new Unifier[T] {
          type Out = tp.Out
          def apply(t: T): Out = unifier(gen.to(t)).tupled
        }
  }

  /**
   * Type class supporting unification of all elements that are subtypes of `B` in this tuple to `B`, with all other
   * elements left unchanged.
   * 
   * @author Miles Sabin
   */
  trait SubtypeUnifier[T, B] extends DepFn1[T]

  object SubtypeUnifier {
    type Aux[T, B, Out0] = SubtypeUnifier[T, B] { type Out = Out0 }
    implicit def subtypeUnifier[T, B, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], unifier: hl.SubtypeUnifier.Aux[L1, B, L2], tp: hl.Tupler[L2]): Aux[T, B, tp.Out] =
        new SubtypeUnifier[T, B] {
          type Out = tp.Out
          def apply(t: T): Out = unifier(gen.to(t)).tupled
        }
  }

  /**
   * Type class supporting computing the type-level Nat corresponding to the length of this tuple. 
   * 
   * @author Miles Sabin
   */
  trait Length[T] extends DepFn1[T]

  object Length {
    type Aux[T, Out0] = Length[T] { type Out = Out0 }
    implicit def length[T, L <: HList]
      (implicit gen: Generic.Aux[T, L], length: hl.Length[L]): Aux[T, length.Out] =
        new Length[T] {
          type Out = length.Out
          def apply(t: T): Out = length()
        }
  }

  /**
   * Type class supporting conversion of this tuple to a `List` with elements typed as the least upper bound
   * of the types of the elements of this tuple.
   * 
   * @author Miles Sabin
   */
  trait ToList[T, Lub] extends DepFn1[T]

  object ToList {
    type Aux[T, Lub, Out0] = ToList[T, Lub] { type Out = Out0 }
    implicit def toList[T, L <: HList, Lub]
      (implicit gen: Generic.Aux[T, L], toList: hl.ToList[L, Lub]): Aux[T, Lub, List[Lub]] =
        new ToList[T, Lub] {
          type Out = List[Lub]
          def apply(t: T): Out = gen.to(t).toList[Lub]
        }
  }

  /**
   * Type class supporting conversion of this tuple to an `Array` with elements typed as the least upper bound
   * of the types of the elements of this tuple.
   * 
   * @author Miles Sabin
   */
  trait ToArray[T, Lub] extends DepFn1[T]

  object ToArray {
    type Aux[T, Lub, Out0] = ToArray[T, Lub] { type Out = Out0 }
    implicit def toArray[T, L <: HList, Lub]
      (implicit gen: Generic.Aux[T, L], toArray: hl.ToArray[L, Lub]): Aux[T, Lub, Array[Lub]] =
        new ToArray[T, Lub] {
          type Out = Array[Lub]
          def apply(t: T): Out = gen.to(t).toArray[Lub]
        }
  }
}
