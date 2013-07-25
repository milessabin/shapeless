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
  trait IsComposite[P] {
    type H
    type T
      
    def head(p : P) : H
    def tail(p : P) : T
  }

  object IsComposite {
    type Aux[P, H0, T0] = IsComposite[P] { type H = H0; type T = T0 }
    implicit def isComposite[P, L <: HList, H0, T <: HList]
      (implicit gen: GenericAux[P, L], isHCons: IsHConsAux[L, H0, T], tp: Tupler[T]): Aux[P, H0, tp.Out] =
        new IsComposite[P] {
          type H = H0
          type T = tp.Out
          def head(p: P): H = isHCons.head(gen.to(p))
          def tail(p: P): T = tp(isHCons.tail(gen.to(p)))
        }
  }

  trait TuplePrepend[T, U] extends DefFn2[T, U]

  object TuplePrepend {
    type Aux[T, U, Out0] = TuplePrepend[T, U] { type Out = Out0 }
    implicit def prepend[T, L1 <: HList, U, L2 <: HList, L3 <: HList]
      (implicit gent: GenericAux[T, L1], genu: GenericAux[U, L2], prepend: PrependAux[L1, L2, L3], tp: Tupler[L3]): Aux[T, U, tp.Out] =
        new TuplePrepend[T, U] {
          type Out = tp.Out
          def apply(t: T, u: U): Out = prepend(gent.to(t), genu.to(u)).tupled
        }
  }

  trait TupleReversePrepend[T, U] extends DefFn2[T, U]

  object TupleReversePrepend {
    type Aux[T, U, Out0] = TupleReversePrepend[T, U] { type Out = Out0 }
    implicit def prepend[T, L1 <: HList, U, L2 <: HList, L3 <: HList]
      (implicit gent: GenericAux[T, L1], genu: GenericAux[U, L2], prepend: ReversePrependAux[L1, L2, L3], tp: Tupler[L3]): Aux[T, U, tp.Out] =
        new TupleReversePrepend[T, U] {
          type Out = tp.Out
          def apply(t: T, u: U): Out = prepend(gent.to(t), genu.to(u)).tupled
        }
  }

  trait TupleAt[T, N <: Nat] extends DepFn1[T]

  object TupleAt {
    type Aux[T, N <: Nat, Out0] = TupleAt[T, N] { type Out = Out0 }
    implicit def at[T, L1 <: HList, N <: Nat]
      (implicit gen: GenericAux[T, L1], at: At[L1, N]): Aux[T, N, at.Out] =
        new TupleAt[T, N] {
          type Out = at.Out
          def apply(t: T): Out = at(gen.to(t))
        }
  }

  trait TupleLast[T] extends DepFn1[T]

  object TupleLast {
    type Aux[T, Out0] = TupleLast[T] { type Out = Out0 }
    implicit def last[T, L <: HList]
      (implicit gen: GenericAux[T, L], last: Last[L]): Aux[T, last.Out] =
        new TupleLast[T] {
          type Out = last.Out
          def apply(t: T): Out = gen.to(t).last
        }
  }

  trait TupleInit[T] extends DepFn1[T]

  object TupleInit {
    type Aux[T, Out0] = TupleInit[T] { type Out = Out0 }
    implicit def init[T, L1 <: HList, L2 <: HList]
      (implicit gen: GenericAux[T, L1], init: InitAux[L1, L2], tp: Tupler[L2]): Aux[T, tp.Out] =
        new TupleInit[T] {
          type Out = tp.Out
          def apply(t: T): Out = init(gen.to(t)).tupled
        }
  }

  trait TupleSelector[T, U] extends DepFn1[T]

  object TupleSelector {
    type Aux[T, U] = TupleSelector[T, U] { type Out = U }
    implicit def select[T, L <: HList, U]
      (implicit gen: GenericAux[T, L], selector: Selector[L, U]): Aux[T, U] =
        new TupleSelector[T, U] {
          type Out = U
          def apply(t: T): Out = gen.to(t).select[U]
        }
  }

  trait TupleFilter[T, U] extends DepFn1[T]

  object TupleFilter {
    type Aux[T, U, Out0] = TupleFilter[T, U] { type Out = Out0 }
    implicit def filterTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: GenericAux[T, L1], filter: FilterAux[L1, U, L2], tp: Tupler[L2]): Aux[T, U, tp.Out] = new TupleFilter[T, U] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filter(gen.to(t)))
      }
  }

  trait TupleFilterNot[T, U] extends DepFn1[T]

  object TupleFilterNot {
    type Aux[T, U, Out0] = TupleFilterNot[T, U] { type Out = Out0 }
    implicit def filterNotTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: GenericAux[T, L1], filterNot: FilterNotAux[L1, U, L2], tp: Tupler[L2]): Aux[T, U, tp.Out] = new TupleFilterNot[T, U] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filterNot(gen.to(t)))
      }
  }

  trait TupleRemove[T, U] extends DepFn1[T]

  object TupleRemove {
    type Aux[T, U, Out0] = TupleRemove[T, U] { type Out = Out0 }
    implicit def removeTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: GenericAux[T, L1], remove: RemoveAux[L1, U, L2], tp: Tupler[L2]): Aux[T, U, (U, tp.Out)] = new TupleRemove[T, U] {
        type Out = (U, tp.Out)
        def apply(t: T): Out = { val (u, rem) = remove(gen.to(t)) ; (u, tp(rem)) }
      }
  }

  trait TupleRemoveAll[T, S] extends DepFn1[T]

  object TupleRemoveAll {
    type Aux[T, S, Out0] = TupleRemoveAll[T, S] { type Out = Out0 }
    implicit def removeAllTuple[T, ST, SL <: HList, L1 <: HList, L2 <: HList]
      (implicit gent: GenericAux[T, L1], gens: GenericAux[ST, SL],  removeAll: RemoveAllAux[SL, L1, L2], tp: Tupler[L2]): Aux[T, ST, (ST, tp.Out)] =
        new TupleRemoveAll[T, ST] {
          type Out = (ST, tp.Out)
          def apply(t: T): Out = { val (e, rem) = removeAll(gent.to(t)) ; (gens.from(e), tp(rem)) }
        }
  }

  trait TupleReplacer[T, U, V] extends DefFn2[T, U]

  object TupleReplacer {
    type Aux[T, U, V, Out0] = TupleReplacer[T, U, V] { type Out = Out0 }
    implicit def replaceTuple[T, L1 <: HList, U, V, L2 <: HList]
      (implicit gen: GenericAux[T, L1], replace: ReplacerAux[L1, V, U, L2], tp: Tupler[L2]): Aux[T, U, V, (V, tp.Out)] = new TupleReplacer[T, U, V] {
        type Out = (V, tp.Out)
        def apply(t: T, u: U): Out = { val (v, rep) = replace(gen.to(t), u) ; (v, tp(rep)) }
      }
  }

  trait TupleReplaceAt[T, N <: Nat, U] extends DefFn2[T, U]

  object TupleReplaceAt {
    type Aux[T, N <: Nat, U, Out0] = TupleReplaceAt[T, N, U] { type Out = Out0 }
    implicit def replaceTuple[T, L1 <: HList, N <: Nat, U, V, L2 <: HList]
      (implicit gen: GenericAux[T, L1], replaceAt: ReplaceAtAux[L1, N, V, U, L2], tp: Tupler[L2]): Aux[T, N, U, (V, tp.Out)] = new TupleReplaceAt[T, N, U] {
        type Out = (V, tp.Out)
        def apply(t: T, u: U): Out = { val (v, rep) = replaceAt(gen.to(t), u) ; (v, tp(rep)) }
      }
  }

  trait TupleTake[T, N <: Nat] extends DepFn1[T]

  object TupleTake {
    type Aux[T, N <: Nat, Out0] = TupleTake[T, N] { type Out = Out0 }
    implicit def tupleTake[T, L1 <: HList, N <: Nat, L2 <: HList]
      (implicit gen: GenericAux[T, L1], take: TakeAux[L1, N, L2], tp: Tupler[L2]): Aux[T, N, tp.Out] = 
        new TupleTake[T, N] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(take(gen.to(t)))
        }
  }

  trait TupleDrop[T, N <: Nat] extends DepFn1[T]

  object TupleDrop {
    type Aux[T, N <: Nat, Out0] = TupleDrop[T, N] { type Out = Out0 }
    implicit def tupleDrop[T, L1 <: HList, N <: Nat, L2 <: HList]
      (implicit gen: GenericAux[T, L1], drop: DropAux[L1, N, L2], tp: Tupler[L2]): Aux[T, N, tp.Out] =
        new TupleDrop[T, N] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(drop(gen.to(t)))
        }
  }

  trait TupleSplit[T, N <: Nat] extends DepFn1[T]

  object TupleSplit {
    import HList.SplitAux
    type Aux[T, N <: Nat, Out0] = TupleSplit[T, N] { type Out = Out0 }
    implicit def tupleSplit[T, L <: HList, N <: Nat, LP <: HList, LS <: HList]
      (implicit gen: GenericAux[T, L], split: SplitAux[L, N, LP, LS], tpp: Tupler[LP], tps: Tupler[LS]): Aux[T, N, (tpp.Out, tps.Out)] =
        new TupleSplit[T, N] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(HNil, gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  trait TupleReverseSplit[T, N <: Nat] extends DepFn1[T]

  object TupleReverseSplit {
    import HList.ReverseSplitAux
    type Aux[T, N <: Nat, Out0] = TupleReverseSplit[T, N] { type Out = Out0 }
    implicit def tupleReverseSplit[T, L <: HList, N <: Nat, LP <: HList, LS <: HList]
      (implicit gen: GenericAux[T, L], split: ReverseSplitAux[L, N, LP, LS], tpp: Tupler[LP], tps: Tupler[LS]): Aux[T, N, (tpp.Out, tps.Out)] =
        new TupleReverseSplit[T, N] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(HNil, gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  trait TupleSplitLeft[T, U] extends DepFn1[T]

  object TupleSplitLeft {
    import HList.SplitLeftAux
    type Aux[T, U, Out0] = TupleSplitLeft[T, U] { type Out = Out0 }
    implicit def tupleSplitLeft[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: GenericAux[T, L], split: SplitLeftAux[L, U, LP, LS], tpp: Tupler[LP], tps: Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new TupleSplitLeft[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(HNil, gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  trait TupleReverseSplitLeft[T, U] extends DepFn1[T]

  object TupleReverseSplitLeft {
    import HList.ReverseSplitLeftAux
    type Aux[T, U, Out0] = TupleReverseSplitLeft[T, U] { type Out = Out0 }
    implicit def tupleReverseSplitLeft[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: GenericAux[T, L], split: ReverseSplitLeftAux[L, U, LP, LS], tpp: Tupler[LP], tps: Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new TupleReverseSplitLeft[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(HNil, gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  trait TupleSplitRight[T, U] extends DepFn1[T]

  object TupleSplitRight {
    import HList.SplitRightAux
    type Aux[T, U, Out0] = TupleSplitRight[T, U] { type Out = Out0 }
    implicit def tupleSplitRight[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: GenericAux[T, L], split: SplitRightAux[L, U, LP, LS], tpp: Tupler[LP], tps: Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new TupleSplitRight[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t), HNil, HNil) ; (tpp(p), tps(s)) }
        }
  }

  trait TupleReverseSplitRight[T, U] extends DepFn1[T]

  object TupleReverseSplitRight {
    import HList.ReverseSplitRightAux
    type Aux[T, U, Out0] = TupleReverseSplitRight[T, U] { type Out = Out0 }
    implicit def tupleReverseSplitRight[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit gen: GenericAux[T, L], split: ReverseSplitRightAux[L, U, LP, LS], tpp: Tupler[LP], tps: Tupler[LS]): Aux[T, U, (tpp.Out, tps.Out)] =
        new TupleReverseSplitRight[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val (p, s) = split(gen.to(t), HNil, HNil) ; (tpp(p), tps(s)) }
        }
  }

  trait TupleReverse[T] extends DepFn1[T]

  object TupleReverse {
    import HList.ReverseAux
    type Aux[T, Out0] = TupleReverse[T] { type Out = Out0 }
    implicit def tupleReverseAux[T, L1 <: HList, L2 <: HList, Out]
      (implicit gen: GenericAux[T, L1], reverse: ReverseAux[L1, L2], tp: Tupler[L2]): Aux[T, tp.Out] =
        new TupleReverse[T] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(reverse(HNil, gen.to(t)))
        }
  }

  trait TupleMapper[T, P] extends DepFn1[T]

  object TupleMapper {
    type Aux[T, P, Out0] = TupleMapper[T, P] { type Out = Out0 }
    implicit def mapper[T, P, L1 <: HList, L2 <: HList]
      (implicit gen: GenericAux[T, L1], mapper: MapperAux[P, L1, L2], tp: Tupler[L2]): Aux[T, P, tp.Out] =
        new TupleMapper[T, P] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(mapper(gen.to(t)))
        }
  }

  trait TupleFlatMapper[T, P] extends DepFn1[T]

  object TupleFlatMapper {
    type Aux[T, P, Out0] = TupleFlatMapper[T, P] { type Out = Out0 }
    implicit def mapper[T, P, L1 <: HList, L2 <: HList]
      (implicit gen: GenericAux[T, L1], mapper: FlatMapperAux[Compose[productElements.type, P], L1, L2], tp: Tupler[L2]): Aux[T, P, tp.Out] =
        new TupleFlatMapper[T, P] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(mapper(gen.to(t)))
        }
  }

  trait TupleConstMapper[T, C] extends DefFn2[T, C]

  object TupleConstMapper {
    type Aux[T, C, Out0] = TupleConstMapper[T, C] { type Out = Out0 }
    implicit def mapper[T, C, L1 <: HList, L2 <: HList]
      (implicit gen: GenericAux[T, L1], mapper: ConstMapperAux[C, L1, L2], tp: Tupler[L2]): Aux[T, C, tp.Out] =
        new TupleConstMapper[T, C] {
          type Out = tp.Out
          def apply(t: T, c: C): tp.Out = tp(mapper(c, gen.to(t)))
        }
  }

  trait TupleMapFolder[T, R, P] { // Nb. Not a dependent function signature
    def apply(t: T, in: R, op: (R, R) => R): R 
  }

  object TupleMapFolder {
    implicit def mapper[T, L <: HList, R, P]
      (implicit gen: GenericAux[T, L], mapper: MapFolder[L, R, P]): TupleMapFolder[T, R, P] =
        new TupleMapFolder[T, R, P] {
          def apply(t: T, in: R, op: (R, R) => R): R = mapper(gen.to(t), in, op)
        }
  }

  trait TupleLeftFolder[T, U, P] extends DefFn2[T, U]

  object TupleLeftFolder {
    type Aux[T, U, P, Out0] = TupleLeftFolder[T, U, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, U, P]
      (implicit gen: GenericAux[T, L], folder: LeftFolder[L, U, P]): Aux[T, U, P, folder.Out] =
        new TupleLeftFolder[T, U, P] {
          type Out = folder.Out
          def apply(t: T, u: U): Out = folder(gen.to(t), u)
        }
  }

  trait TupleRightFolder[T, U, P] extends DefFn2[T, U]

  object TupleRightFolder {
    type Aux[T, U, P, Out0] = TupleRightFolder[T, U, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, U, P]
      (implicit gen: GenericAux[T, L], folder: RightFolder[L, U, P]): Aux[T, U, P, folder.Out] =
        new TupleRightFolder[T, U, P] {
          type Out = folder.Out
          def apply(t: T, u: U): Out = folder(gen.to(t), u)
        }
  }
  
  trait TupleLeftReducer[T, P] extends DepFn1[T]

  object TupleLeftReducer {
    type Aux[T, P, Out0] = TupleLeftReducer[T, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, P]
      (implicit gen: GenericAux[T, L], folder: LeftReducer[L, P]): Aux[T, P, folder.Out] =
        new TupleLeftReducer[T, P] {
          type Out = folder.Out
          def apply(t: T): Out = folder(gen.to(t))
        }
  }
  
  trait TupleRightReducer[T, P] extends DepFn1[T]

  object TupleRightReducer {
    type Aux[T, P, Out0] = TupleRightReducer[T, P] { type Out = Out0 }
    implicit def folder[T, L <: HList, P]
      (implicit gen: GenericAux[T, L], folder: RightReducer[L, P]): Aux[T, P, folder.Out] =
        new TupleRightReducer[T, P] {
          type Out = folder.Out
          def apply(t: T): Out = folder(gen.to(t))
        }
  }

  trait TupleTransposer[T] extends DepFn1[T]

  object TupleTransposer {
    type Aux[T, Out0] = TupleTransposer[T] { type Out = Out0 }
    implicit def transpose[T, L1 <: HList, L2 <: HList, L3 <: HList, L4 <: HList]
      (implicit
        gen: GenericAux[T, L1],
        mpe: MapperAux[productElements.type, L1, L2],
        tps: TransposerAux[L2, L3],
        mtp: MapperAux[tupled.type, L3, L4],
        tp:  Tupler[L4]
      ): Aux[T, tp.Out] =
      new TupleTransposer[T] {
        type Out = tp.Out
        def apply(t: T): Out = ((gen.to(t) map productElements).transpose map tupled).tupled
      }
  }

  trait TupleZipApply[FT, AT] extends DefFn2[FT, AT]

  object TupleZipApply {
    type Aux[FT, AT, Out0] = TupleZipApply[FT, AT] { type Out = Out0 }
    implicit def zipApply[FT, FL <: HList, AT, AL <: HList, RL <: HList]
      (implicit
        genf: GenericAux[FT, FL],
        gena: GenericAux[AT, AL],
        zip:  ZipApplyAux[FL, AL, RL],
        tp:   Tupler[RL]
      ): Aux[FT, AT, tp.Out] = 
      new TupleZipApply[FT, AT] {
        type Out = tp.Out
        def apply(ft: FT, at: AT): Out = (genf.to(ft) zipApply gena.to(at)).tupled
      }
  }

  trait TupleZipOne[H, T] extends DefFn2[H, T]

  object TupleZipOne {
    type Aux[H, T, Out0] = TupleZipOne[H, T] { type Out = Out0 }
    implicit def zipOne[HT, HL <: HList, TT, TL <: HList, TLL <: HList, RLL <: HList, RL <: HList]
      (implicit
        genh: GenericAux[HT, HL],
        gent: GenericAux[TT, TL],
        mpet: MapperAux[productElements.type, TL, TLL],
        zone: ZipOneAux[HL, TLL, RLL],
        mtp:  MapperAux[tupled.type, RLL, RL],
        tp:   Tupler[RL]
      ): Aux[HT, TT, tp.Out] =
      new TupleZipOne[HT, TT] {
        type Out = tp.Out
        def apply(h: HT, t: TT): Out = ((genh.to(h) zipOne (gent.to(t) map productElements)) map tupled).tupled
      }
  }

  trait TupleUnifier[T] extends DepFn1[T]

  object TupleUnifier {
    type Aux[T, Out0] = TupleUnifier[T] { type Out = Out0 }
    implicit def unifier[T, L1 <: HList, L2 <: HList]
      (implicit gen: GenericAux[T, L1], unifier: UnifierAux[L1, L2], tp: Tupler[L2]): Aux[T, tp.Out] =
        new TupleUnifier[T] {
          type Out = tp.Out
          def apply(t: T): Out = unifier(gen.to(t)).tupled
        }
  }

  trait TupleSubtypeUnifier[T, B] extends DepFn1[T]

  object TupleSubtypeUnifier {
    type Aux[T, B, Out0] = TupleSubtypeUnifier[T, B] { type Out = Out0 }
    implicit def subtypeUnifier[T, B, L1 <: HList, L2 <: HList]
      (implicit gen: GenericAux[T, L1], unifier: SubtypeUnifierAux[L1, B, L2], tp: Tupler[L2]): Aux[T, B, tp.Out] =
        new TupleSubtypeUnifier[T, B] {
          type Out = tp.Out
          def apply(t: T): Out = unifier(gen.to(t)).tupled
        }
  }

  trait TupleLength[T] extends DepFn1[T]

  object TupleLength {
    type Aux[T, Out0] = TupleLength[T] { type Out = Out0 }
    implicit def length[T, L <: HList]
      (implicit gen: GenericAux[T, L], length: Length[L]): Aux[T, length.Out] =
        new TupleLength[T] {
          type Out = length.Out
          def apply(t: T): Out = length()
        }
  }

  trait TupleToList[T, Lub] extends DepFn1[T]

  object TupleToList {
    type Aux[T, Lub, Out0] = TupleToList[T, Lub] { type Out = Out0 }
    implicit def toList[T, L <: HList, Lub]
      (implicit gen: GenericAux[T, L], toList: ToList[L, Lub]): Aux[T, Lub, List[Lub]] =
        new TupleToList[T, Lub] {
          type Out = List[Lub]
          def apply(t: T): Out = gen.to(t).toList[Lub]
        }
  }

  trait TupleToArray[T, Lub] extends DepFn1[T]

  object TupleToArray {
    type Aux[T, Lub, Out0] = TupleToArray[T, Lub] { type Out = Out0 }
    implicit def toArray[T, L <: HList, Lub]
      (implicit gen: GenericAux[T, L], toArray: ToArray[L, Lub]): Aux[T, Lub, Array[Lub]] =
        new TupleToArray[T, Lub] {
          type Out = Array[Lub]
          def apply(t: T): Out = gen.to(t).toArray[Lub]
        }
  }
}
