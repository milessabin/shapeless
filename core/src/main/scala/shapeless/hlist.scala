/*
 * Copyright (c) 2011 Miles Sabin 
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

import scala.annotation.tailrec

import TypeOperators._  
  
/**
 * `HList` ADT base trait.
 * 
 * @author Miles Sabin
 */
sealed trait HList

/**
 * Non-empty `HList` element type.
 * 
 * @author Miles Sabin
 */
final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
  override def toString = head+" :: "+tail.toString
}

/**
 * Empty `HList` element type.
 * 
 * @author Miles Sabin
 */
sealed trait HNil extends HList {
  def ::[H](h : H) = shapeless.::(h, this)
  override def toString = "HNil"
}

/**
 * Empty `HList` value.
 * 
 * @author Miles Sabin
 */
case object HNil extends HNil

/**
 * Carrier for `HList` operations.
 * 
 * These methods are implemented here and pimped onto the minimal `HList` types to avoid issues that would otherwise be
 * caused by the covariance of `::[H, T]`.
 * 
 * @author Miles Sabin
 */
final class HListOps[L <: HList](l : L) {
  /**
   * Returns the head of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  def head(implicit c : IsHCons[L]) : c.H = c.head(l) 

  /**
   * Returns that tail of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  def tail(implicit c : IsHCons[L]) : c.T = c.tail(l)
  
  /**
   * Prepend the argument element to this `HList`.
   */
  def ::[H](h : H) : H :: L = shapeless.::(h, l)

  /**
   * Prepend the argument element to this `HList`.
   */
  def +:[H](h : H) : H :: L = shapeless.::(h, l)
  
  /**
   * Append the argument element to this `HList`.
   */
  def :+[T](t : T)(implicit prepend : Prepend[L, T :: HNil]) : prepend.Out = prepend(l, t :: HNil)
  
  /**
   * Append the argument `HList` to this `HList`.
   */
  def ++[S <: HList](suffix : S)(implicit prepend : Prepend[L, S]) : prepend.Out = prepend(l, suffix)
  
  /**
   * Prepend the argument `HList` to this `HList`.
   */
  def ++:[P <: HList](prefix : P)(implicit prepend : Prepend[P, L]) : prepend.Out = prepend(prefix, l)
  
  /**
   * Prepend the argument `HList` to this `HList`.
   */
  def :::[P <: HList](prefix : P)(implicit prepend : Prepend[P, L]) : prepend.Out = prepend(prefix, l)
  
  /**
   * Prepend the reverse of the argument `HList` to this `HList`.
   */
  def reverse_:::[P <: HList](prefix : P)(implicit prepend : ReversePrepend[P, L]) : prepend.Out = prepend(prefix, l)

  /**
   * Returns the ''nth'' of this `HList`. An explicit type argument must be provided. Available only if there is
   * evidence that this `HList` has at least ''n'' elements.
   */
  def apply[N <: Nat](implicit at : At[L, N]) : at.Out = at(l)

  /**
   * Returns the ''nth'' of this `HList`. Available only if there is evidence that this `HList` has at least ''n''
   * elements.
   */
  def apply[N <: Nat](n : N)(implicit at : At[L, N]) : at.Out = at(l)
  
  /**
   * Returns the last element of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  def last(implicit last : Last[L]) : last.Out = last(l)

  /**
   * Returns an `HList` consisting of all the elements of this `HList` except the last. Available only if there is
   * evidence that this `HList` is composite.
   */
  def init(implicit init : Init[L]) : init.Out = init(l)
  
  /**
   * Returns the first element of type `U` of this `HList`. An explicit type argument must be provided. Available only
   * if there is evidence that this `HList` has an element of type `U`.
   */
  def select[U](implicit selector : Selector[L, U]) : U = selector(l)

  /**
   * Returns all elements of type `U` of this `HList`. An explicit type argument must be provided.
   */
  def filter[U](implicit filter : Filter[L, U]) : filter.Out  = filter(l)

  /**
   * Returns all elements of type different than `U` of this `HList`. An explicit type argument must be provided.
   */
  def filterNot[U](implicit filter : FilterNot[L, U]) : filter.Out  = filter(l)
  
  /**
   * Returns the first element of type `U` of this `HList` plus the remainder of the `HList`. An explicit type argument
   * must be provided. Available only if there is evidence that this `HList` has an element of type `U`.
   * 
   * The `Elem` suffix is here to avoid creating an ambiguity with RecordOps#remove and should be removed if
   * SI-5414 is resolved in a way which eliminates the ambiguity.
   */
  def removeElem[U](implicit remove : Remove[U, L]) : (U, remove.Out) = remove(l)
  
  /**
   * Returns the first elements of this `HList` that have types in `SL` plus the remainder of the `HList`. An expicit
   * type argument must be provided. Available only if there is evidence that this `HList` contains elements with
   * types in `SL`.
   */
  def removeAll[SL <: HList](implicit removeAll : RemoveAll[SL, L]) : (SL, removeAll.Out) = removeAll(l)

  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value, also of type `U` returning both
   * the replaced element and the updated `HList`. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  def replace[U](u : U)(implicit replacer : Replacer[L, U, U]) : (U, replacer.Out) = replacer(l, u)
  
  class ReplaceTypeAux[U] {
    def apply[V](v : V)(implicit replacer : Replacer[L, U, V]) : (U, replacer.Out) = replacer(l, v)
  }
  
  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value of type `V`, return both the
   * replaced element and the updated `HList`. An explicit type argument must be provided for `U`. Available only if
   * there is evidence that this `HList` has an element of type `U`.
   */
  def replaceType[U] = new ReplaceTypeAux[U]
  
  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value, also of type `U`. Available only
   * if there is evidence that this `HList` has an element of type `U`.
   * 
   * The `Elem` suffix is here to avoid creating an ambiguity with RecordOps#updated and should be removed if
   * SI-5414 is resolved in a way which eliminates the ambiguity.
   */
  def updatedElem[U](u : U)
    (implicit replacer : Replacer[L, U, U]) : replacer.Out = replacer(l, u)._2
  
  class UpdatedTypeAux[U] {
    def apply[V](v : V)(implicit replacer : Replacer[L, U, V]) : replacer.Out = replacer(l, v)._2
  }
  
  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value of type `V`. An explicit type
   * argument must be provided for `U`. Available only if there is evidence that this `HList` has an element of
   * type `U`.
   */
  def updatedType[U] = new UpdatedTypeAux[U]
  
  class UpdatedAtAux[N <: Nat] {
    def apply[U](u : U)(implicit replacer : ReplaceAt[L, N, U]) : replacer.Out = replacer(l, u)._2
  }
  
  /**
   * Replaces the first element of type `U` of this `HList` with the supplied value of type `V`. An explicit type
   * argument must be provided for `U`. Available only if there is evidence that this `HList` has an element of
   * type `U`.
   */
  def updatedAt[N <: Nat] = new UpdatedAtAux[N]
  
  /**
   * Returns the first ''n'' elements of this `HList`. An explicit type argument must be provided. Available only if
   * there is evidence that this `HList` has at least ''n'' elements.
   */
  def take[N <: Nat](implicit take : Take[L, N]) : take.Out = take(l)

  /**
   * Returns the first ''n'' elements of this `HList`. Available only if there is evidence that this `HList` has at
   * least ''n'' elements.
   */
  def take[N <: Nat](n : N)(implicit take : Take[L, N]) : take.Out = take(l)
  
  /**
   * Returns all but the  first ''n'' elements of this `HList`. An explicit type argument must be provided. Available
   * only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def drop[N <: Nat](implicit drop : Drop[L, N]) : drop.Out = drop(l)

  /**
   * Returns all but the  first ''n'' elements of this `HList`. Available only if there is evidence that this `HList`
   * has at least ''n'' elements.
   */
  def drop[N <: Nat](n : N)(implicit drop : Drop[L, N]) : drop.Out = drop(l)
  
  /**
   * Splits this `HList` at the ''nth'' element, returning the prefix and suffix as a pair. An explicit type argument
   * must be provided. Available only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def split[N <: Nat](implicit split : Split[L, N]) : split.Out = split(l)

  /**
   * Splits this `HList` at the ''nth'' element, returning the prefix and suffix as a pair. Available only if there is
   * evidence that this `HList` has at least ''n'' elements.
   */
  def split[N <: Nat](n : N)(implicit split : Split[L, N]) : split.Out = split(l)
  
  /**
   * Splits this `HList` at the ''nth'' element, returning the reverse of the prefix and suffix as a pair. An explicit
   * type argument must be provided. Available only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def reverse_split[N <: Nat](implicit split : ReverseSplit[L, N]) : split.Out = split(l)

  /**
   * Splits this `HList` at the ''nth'' element, returning the reverse of the prefix and suffix as a pair. Available
   * only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def reverse_split[N <: Nat](n : N)(implicit split : ReverseSplit[L, N]) : split.Out = split(l)

  /**
   * Splits this `HList` at the first occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  def splitLeft[U](implicit splitLeft : SplitLeft[L, U]) : splitLeft.Out = splitLeft(l)

  /**
   * Splits this `HList` at the first occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `HList` has
   * an element of type `U`.
   */
  def reverse_splitLeft[U](implicit splitLeft : ReverseSplitLeft[L, U]) : splitLeft.Out = splitLeft(l)

  /**
   * Splits this `HList` at the last occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  def splitRight[U](implicit splitRight : SplitRight[L, U]) : splitRight.Out = splitRight(l)

  /**
   * Splits this `HList` at the last occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `HList` has
   * an element of type `U`.
   */
  def reverse_splitRight[U](implicit splitRight : ReverseSplitRight[L, U]) : splitRight.Out = splitRight(l)

  /**
   * Reverses this `HList`.
   */
  def reverse(implicit reverse : Reverse[L]) : reverse.Out = reverse(l)

  /**
   * Maps a higher rank function across this `HList`.
   */
  def map[HF](f : HF)(implicit mapper : Mapper[HF, L]) : mapper.Out = mapper(l)

  /**
   * Flatmaps a higher rank function across this `HList`.
   */
  def flatMap[HF](f : HF)(implicit mapper : FlatMapper[HF, L]) : mapper.Out = mapper(l)

  /**
   * Replaces each element of this `HList` with a constant value.
   */
  def mapConst[C](c : C)(implicit mapper : ConstMapper[C, L]) : mapper.Out = mapper(c, l)
  
  /**
   * Maps a higher rank function ''f'' across this `HList` and folds the result using monomorphic combining operator
   * `op`. Available only if there is evidence that the result type of `f` at each element conforms to the argument
   * type of ''op''.
   */
  def foldMap[R, HF](z : R)(f : HF)(op : (R, R) => R)(implicit folder : MapFolder[L, R, HF]) : R = folder(l, z, op)
  
  /**
   * Computes a left fold over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the partial results of the appropriate types.
   */
  def foldLeft[R, HF](z : R)(op : HF)(implicit folder : LeftFolder[L, R, HF]) : folder.Out = folder(l, z)
  
  /**
   * Computes a right fold over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence `op` can consume/produce all the partial results of the appropriate types.
   */
  def foldRight[R, HF](z : R)(op : HF)(implicit folder : RightFolder[L, R, HF]) : folder.Out = folder(l, z)
  
  /**
   * Computes a left reduce over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence that this `HList` has at least one element and that `op` can consume/produce all the partial
   * results of the appropriate types.
   */
  def reduceLeft[HF](op : HF)(implicit reducer : LeftReducer[L, HF]) : reducer.Out = reducer(l)
  
  /**
   * Computes a right reduce over this `HList` using the polymorphic binary combining operator `op`. Available only if
   * there is evidence that this `HList` has at least one element and that `op` can consume/produce all the partial
   * results of the appropriate types.
   */
  def reduceRight[HF](op : HF)(implicit reducer : RightReducer[L, HF]) : reducer.Out = reducer(l)
  
  /**
   * Zips this `HList` with its argument `HList` returning an `HList` of pairs.
   */
  def zip[R <: HList](r : R)(implicit zipper : Zip[L :: R :: HNil]) : zipper.Out = zipper(l :: r :: HNil)
  
  /**
   * Zips this `HList` of monomorphic function values with its argument `HList` of correspondingly typed function
   * arguments returning the result of each application as an `HList`. Available only if there is evidence that the
   * corresponding function and argument elements have compatible types.
   */
  def zipApply[A <: HList](a : A)(implicit zipper : ZipApply[L, A]) : zipper.Out = zipper(l, a)

  /**
   * Zips this `HList` of `HList`s returning an `HList` of tuples. Available only if there is evidence that this
   * `HList` has `HList` elements.
   */
  def zipped(implicit zipper : Zip[L]) : zipper.Out = zipper(l)

  /**
   * Unzips this `HList` of tuples returning a tuple of `HList`s. Available only if there is evidence that this
   * `HList` has tuple elements.
   */
  def unzipped(implicit unzipper : Unzip[L]) : unzipper.Out = unzipper(l)
  
  /**
   * Zips this `HList` with its argument `HList` of `HList`s, returning an `HList` of `HList`s with each element of
   * this `HList` prepended to the corresponding `HList` element of the argument `HList`.
   */
  def zipOne[T <: HList](t : T)(implicit zipOne : ZipOne[L, T]) : zipOne.Out = zipOne(l, t)
  
  /**
   * Transposes this `HList`.
   */
  def transpose(implicit transpose : Transposer[L]) : transpose.Out = transpose(l)

  /**
   * Returns an `HList` typed as a repetition of the least upper bound of the types of the elements of this `HList`.
   */
  def unify(implicit unifier : Unifier[L]) : unifier.Out = unifier(l)

  /**
   * Returns an `HList` with all elements that are subtypes of `B` typed as `B`.
   */
  def unifySubtypes[B](implicit subtypeUnifier : SubtypeUnifier[L, B]) : subtypeUnifier.Out = subtypeUnifier(l)

  /**
   * Converts this `HList` to a correspondingly typed tuple.
   */
  def tupled(implicit tupler : Tupler[L]) : tupler.Out = tupler(l)
  
  /**
   * Compute the length of this `HList`.
   */
  def length(implicit length : Length[L]) : length.Out = length()
  
  /**
   * Compute the length of this `HList` as a runtime Int value.
   */
  def runtimeLength: Int = {
    @tailrec def loop(l: HList, acc: Int): Int = l match {
      case HNil => acc
      case hd :: tl => loop(tl, acc+1)
    }

    loop(l, 0)
  }

  /**
   * Converts this `HList` to an ordinary `List` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   */
  def toList[Lub](implicit toList : ToList[L, Lub]) : List[Lub] = toList(l)
  
  /**
   * Converts this `HList` to an `Array` of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   * 
   * It is advisable to specify the type parameter explicitly, because for many reference types, case classes in
   * particular, the inferred type will be too precise (ie. `Product with Serializable with CC` for a typical case class
   * `CC`) which interacts badly with the invariance of `Array`s.
   */
  def toArray[Lub](implicit toArray : ToArray[L, Lub]) : Array[Lub] = toArray(0, l)
}

object HList {
  def apply() = HNil
  
  def apply[P <: Product, L <: HList](p : P)(implicit hl : HListerAux[P, L]) : L = hl(p)
  
  implicit def hlistOps[L <: HList](l : L) : HListOps[L] = new HListOps(l)

  /**
   * Convenience aliases for HList :: and List :: allowing them to be used together within match expressions.  
   */
  object ListCompat {
    val :: = scala.collection.immutable.::
    val #: = shapeless.::
  }

  type SplitAux[L <: HList, N <: Nat, P <: HList, S <: HList] = Split0[HNil, L, N, P, S]
  
  type ReverseSplitAux[L <: HList, N <: Nat, P <: HList, S <: HList] = ReverseSplit0[HNil, L, N, P, S]
  
  type SplitLeftAux[L <: HList, U, P <: HList, S <: HList] = SplitLeft0[HNil, L, U, P, S]
  
  type ReverseSplitLeftAux[L <: HList, U, P <: HList, S <: HList] = ReverseSplitLeft0[HNil, L, U, P, S]
  
  type SplitRightAux[L <: HList, U, P <: HList, S <: HList] = SplitRight0[L, HNil, HNil, U, P, S]
  
  type ReverseSplitRightAux[L <: HList, U, P <: HList, S <: HList] = ReverseSplitRight0[L, HNil, HNil, U, P, S]
  
  type ReverseAux[L <: HList, Out <: HList] = Reverse0[HNil, L, Out]
}

/**
 * Type class witnessing that this `HList` is composite and providing access to head and tail. 
 * 
 * @author Miles Sabin
 */
trait IsHCons[L <: HList] {
  type H
  type T <: HList
    
  def head(l : L) : H
  def tail(l : L) : T
}

object IsHCons {
  implicit def hlistIsHCons[H0, T0 <: HList] = new IsHCons[H0 :: T0] {
    type H = H0
    type T = T0
  
    def head(l : H0 :: T0) : H = l.head
    def tail(l : H0 :: T0) : T = l.tail
  }
}

/**
 * Type class witnessing that the result of wrapping each element of `HList` `L` in type constructor `F` is `Out`.
 */
trait Mapped[L <: HList, F[_]] {
  type Out <: HList
}

object Mapped {
  implicit def mapped[L <: HList, F[_], Out0 <: HList](implicit mapped : MappedAux[L, F, Out0]) = new Mapped[L, F] {
    type Out = Out0
  }
}

trait MappedAux[L <: HList, F[_], Out <: HList]

object MappedAux {
  implicit def hnilMappedAux[F[_]] = new MappedAux[HNil, F, HNil] {}
  
  implicit def hlistIdMapped[L <: HList] = new MappedAux[L, Id, L] {}
  
  implicit def hlistMappedAux1[H, T <: HList, F[_], OutT <: HList](implicit mt : MappedAux[T, F, OutT]) =
    new MappedAux[H :: T, F, F[H] :: OutT] {}

  implicit def hlistMappedAux2[H, T <: HList, F, OutT <: HList](implicit mt : MappedAux[T, Const[F]#λ, OutT]) =
    new MappedAux[H :: T, Const[F]#λ, F :: OutT] {}
}

/**
 * Type class witnessing that the result of stripping type constructor `F` off each element of `HList` `L` is `Out`.
 */
trait Comapped[L <: HList] {
  type Out <: HList
  type F[_]
}

object Comapped {
  implicit def comapped[L <: HList, F0[_], Out0 <: HList](implicit mapped: ComappedAux[L, F0, Out0]) = new Comapped[L] {
    type Out = Out0
    type F[X] = F0[X]
  }
}

trait ComappedAux[L <: HList, F[_], Out <: HList]

trait LowPriorityComappedAux {
  implicit def hlistIdComapped[L <: HList] = new ComappedAux[L, Id, L] {}
}

object ComappedAux extends LowPriorityComappedAux {
  implicit def hnilComappedAux[F[_]] = new ComappedAux[HNil, F, HNil] {}

  implicit def hlistComappedAux[H, T <: HList, F[_], OutT <: HList](implicit mt : ComappedAux[T, F, OutT]) =
    new ComappedAux[F[H] :: T, F, H :: OutT] {}
}

/**
 * Type class witnessing that `HList`s `L1` and `L2` have elements of the form `F1[Ln]` and `F2[Ln]` respectively for all
 * indices `n`. This implies that a natural transform `F1 ~> F2` will take a list of type `L1` onto a list of type `L2`.
 * 
 * @author Miles Sabin
 */
trait NatTRel[L1 <: HList, F1[_], L2 <: HList, F2[_]] {
  def map(nt: F1 ~> F2, fa: L1): L2
}

object NatTRel {
  implicit def hnilNatTRel1[F1[_], F2[_]] = new NatTRel[HNil, F1, HNil, F2] {
    def map(f: F1 ~> F2, fa: HNil): HNil = HNil
  }
  implicit def hnilNatTRel2[F1[_], H2] = new NatTRel[HNil, F1, HNil, Const[H2]#λ] {
    def map(f: F1 ~> Const[H2]#λ, fa: HNil): HNil = HNil
  }

  implicit def hlistNatTRel1[H, F1[_], F2[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, F2]) =
    new NatTRel[F1[H] :: T1, F1, F2[H] :: T2, F2] {
      def map(f: F1 ~> F2, fa: F1[H] :: T1): F2[H] :: T2 = f(fa.head) :: nt.map(f, fa.tail)
    }

  implicit def hlistNatTRel2[H, F2[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, Id, T2, F2]) =
    new NatTRel[H :: T1, Id, F2[H] :: T2, F2] {
      def map(f: Id ~> F2, fa: H :: T1): F2[H] :: T2 = f(fa.head) :: nt.map(f, fa.tail)
    }

  implicit def hlistNatTRel3[H, F1[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, Id]) =
    new NatTRel[F1[H] :: T1, F1, H :: T2, Id] {
      def map(f: F1 ~> Id, fa: F1[H] :: T1): H :: T2 = f(fa.head) :: nt.map(f, fa.tail)
    }

  implicit def hlistNatTRel4[H1, F1[_], T1 <: HList, H2, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, Const[H2]#λ]) =
    new NatTRel[F1[H1] :: T1, F1, H2 :: T2, Const[H2]#λ] {
      def map(f: F1 ~> Const[H2]#λ, fa: F1[H1] :: T1): H2 :: T2 = f(fa.head) :: nt.map(f, fa.tail)
    }

  implicit def hlistNatTRel5[H1, T1 <: HList, H2, T2 <: HList](implicit nt : NatTRel[T1, Id, T2, Const[H2]#λ]) =
    new NatTRel[H1 :: T1, Id, H2 :: T2, Const[H2]#λ] {
      def map(f: Id ~> Const[H2]#λ, fa: H1 :: T1): H2 :: T2 = f(fa.head) :: nt.map(f, fa.tail)
    }
}

/**
 * Type class providing minimally witnessed operations on `HList`s which can be derived from `L` by wrapping
 * each of its elements in a type constructor.
 */
trait HKernel {
  type L <: HList
  type Mapped[G[_]] <: HList
  type Length <: Nat

  def map[F[_], G[_]](f: F ~> G, l: Mapped[F]): Mapped[G]

  def tabulate[C](from: Int)(f: Int => C): Mapped[Const[C]#λ]

  def toList[C](l: Mapped[Const[C]#λ]): List[C]

  def length: Int
}

trait HNilHKernel extends HKernel {
  import Nat._

  type L = HNil
  type Mapped[G[_]] = HNil
  type Length = _0

  def map[F[_], G[_]](f: F ~> G, l: HNil): HNil = HNil

  def tabulate[C](from: Int)(f: Int => C): HNil = HNil

  def toList[C](l: HNil): List[C] = Nil

  def length: Int = 0
}

case object HNilHKernel extends HNilHKernel

final case class HConsHKernel[H, T <: HKernel](tail: T) extends HKernel {
  type L = H :: tail.L
  type Mapped[G[_]] = G[H] :: tail.Mapped[G]
  type Length = Succ[tail.Length]

  def map[F[_], G[_]](f: F ~> G, l: F[H] :: tail.Mapped[F]): G[H] :: tail.Mapped[G] = f(l.head) :: tail.map(f, l.tail)

  def tabulate[C](from: Int)(f: Int => C): C :: tail.Mapped[Const[C]#λ] = f(from) :: tail.tabulate(from+1)(f)

  def toList[C](l: C :: tail.Mapped[Const[C]#λ]): List[C] = l.head :: tail.toList(l.tail)

  def length: Int = 1+tail.length
}

object HKernel {
  def apply[L <: HList](implicit mk: HKernelAux[L]): mk.Out = mk()
  def apply[L <: HList](l: L)(implicit mk: HKernelAux[L]): mk.Out = mk()
}

trait HKernelAux[L <: HList] {
  type Out <: HKernel
  def apply(): Out
}

object HKernelAux {
  implicit def mkHNilHKernel = new HKernelAux[HNil] {
    type Out = HNilHKernel
    def apply() = HNilHKernel
  }

  implicit def mkHListHKernel[H, T <: HList](implicit ct: HKernelAux[T]) = new HKernelAux[H :: T] {
    type Out = HConsHKernel[H, ct.Out]
    def apply() = HConsHKernel[H, ct.Out](ct())
  }
}

/**
 * Type class supporting computing the type-level Nat corresponding to the length of this `HList`. 
 * 
 * @author Miles Sabin
 */
trait Length[-L <: HList] {
  type Out <: Nat
  def apply() : Out
}

trait LengthAux[-L <: HList, N <: Nat] {
  def apply() : N
}

object Length {
  implicit def length[L <: HList, N <: Nat](implicit length : LengthAux[L, N]) = new Length[L] {
    type Out = N
    def apply() = length()
  }
}

object LengthAux {
  import Nat._
  
  implicit def hnilLength = new LengthAux[HNil, _0] {
    def apply() = _0
  }
  
  implicit def hlistLength[H, T <: HList, N <: Nat](implicit lt : LengthAux[T, N], sn : Succ[N]) = new LengthAux[H :: T, Succ[N]] {
    def apply() = sn
  }
}

/**
 * Type class supporting mapping a higher ranked function over this `HList`. 
 * 
 * @author Miles Sabin
 */
trait Mapper[HF, In <: HList] {
  type Out <: HList
  def apply(in: In) : Out
}

trait MapperAux[HF, In <: HList, Out <: HList] {
  def apply(in: In) : Out
}

object Mapper {
  implicit def mapper[HF, In <: HList, Out0 <: HList](implicit mapper : MapperAux[HF, In, Out0]) = new Mapper[HF, In] {
    type Out = Out0
    def apply(in: In) : Out = mapper(in)
  }
}

object MapperAux {
  import Poly._
  
  implicit def hnilMapper1[HF] = new MapperAux[HF, HNil, HNil] {
    def apply(l : HNil) = HNil
  }
  
  implicit def hlistMapper1[HF <: Poly, InH, OutH, InT <: HList, OutT <: HList]
    (implicit hc : Pullback1Aux[HF, InH, OutH], mt : MapperAux[HF, InT, OutT]) = new MapperAux[HF, InH :: InT, OutH :: OutT] {
      def apply(l : InH :: InT) = hc(l.head) :: mt(l.tail)
  }
}

/**
 * Type class supporting flatmapping a higher ranked function over this `HList`. 
 * 
 * @author Miles Sabin
 */
trait FlatMapper[HF, In <: HList] {
  type Out <: HList
  def apply(in: In) : Out
}

trait FlatMapperAux[HF, In <: HList, Out <: HList] {
  def apply(in: In) : Out
}

object FlatMapper {
  implicit def mapper[HF, In <: HList, Out0 <: HList](implicit mapper : FlatMapperAux[HF, In, Out0]) =
    new FlatMapper[HF, In] {
      type Out = Out0
      def apply(in: In) : Out = mapper(in)
    }
}

object FlatMapperAux {
  import Poly._
  
  implicit def hnilFlatMapper1[HF] = new FlatMapperAux[HF, HNil, HNil] {
    def apply(l : HNil) = HNil
  }
  
  implicit def hlistFlatMapper1[HF <: Poly, InH, OutH <: HList, InT <: HList, OutT <: HList, Out <: HList]
    (implicit
      hc : Pullback1Aux[HF, InH, OutH],
      mt : FlatMapperAux[HF, InT, OutT],
      prepend : PrependAux[OutH, OutT, Out]) =
      new FlatMapperAux[HF, InH :: InT, Out] {
        def apply(l : InH :: InT) = prepend(hc(l.head), mt(l.tail))
      }
}

/**
 * Type class supporting mapping a constant valued function over this `HList`. 
 * 
 * @author Miles Sabin
 */
trait ConstMapper[C, L <: HList] {
  type Out <: HList
  def apply(c : C, l : L) : Out
}
  
trait ConstMapperAux[C, L <: HList, Out <: HList] {
  def apply(c : C, l : L) : Out
}

object ConstMapper {
  implicit def constMapper[L <: HList, C, Out0 <: HList](implicit mc : ConstMapperAux[C, L, Out0]) =
    new ConstMapper[C, L] {
      type Out = Out0
      def apply(c : C, l : L) : Out = mc(c, l)
    } 
}

object ConstMapperAux {
  implicit def hnilConstMapper[C] = new ConstMapperAux[C, HNil, HNil] {
    def apply(c : C, l : HNil) = l 
  }
  
  implicit def hlistConstMapper[H, T <: HList, C, OutT <: HList](implicit mct : ConstMapperAux[C, T, OutT]) =
    new ConstMapperAux[C, H :: T, C :: OutT] {
      def apply(c : C, l : H :: T) : C :: OutT = c :: mct(c, l.tail)  
    }
}  

/**
 * Type class supporting mapping a polymorphic function over this `HList` and then folding the result using a
 * monomorphic function value. 
 * 
 * @author Miles Sabin
 */
trait MapFolder[L <: HList, R, HF] {
  def apply(l : L, in : R, op : (R, R) => R) : R 
}
  
object MapFolder {
  import Poly._
  
  implicit def hnilMapFolder[R, HF] = new MapFolder[HNil, R, HF] {
    def apply(l : HNil, in : R, op : (R, R) => R) = in
  }
  
  implicit def hlistMapFolder[H, T <: HList, R, HF <: Poly]
    (implicit hc : Pullback1Aux[HF, H, R], tf : MapFolder[T, R, HF]) =
      new MapFolder[H :: T, R, HF] {
        def apply(l : H :: T, in : R, op : (R, R) => R) = op(hc(l.head), tf(l.tail, in, op))
      }
}

/**
 * Type class supporting left-folding a polymorphic binary function over this `HList`.
 * 
 * @author Miles Sabin
 */
trait LeftFolder[L <: HList, In, HF] {
  type Out
  def apply(l : L, in : In) : Out 
}

trait LeftFolderAux[L <: HList, In, HF, Out] {
  def apply(l : L, in : In) : Out 
}

object LeftFolder {
  implicit def leftFolder[L <: HList, In, HF, Out0](implicit folder : LeftFolderAux[L, In, HF, Out0]) =
    new LeftFolder[L, In, HF] {
      type Out = Out0
      def apply(l : L, in : In) : Out = folder.apply(l, in)
    }
}

object LeftFolderAux {
  import Poly._
  
  implicit def hnilLeftFolderAux[In, HF] = new LeftFolderAux[HNil, In, HF, In] {
    def apply(l : HNil, in : In) : In = in 
  }
  
  implicit def hlistLeftFolderAux[H, T <: HList, In, HF, OutH, Out]
    (implicit f : Pullback2Aux[HF, In, H, OutH], ft : LeftFolderAux[T, OutH, HF, Out]) =
      new LeftFolderAux[H :: T, In, HF, Out] {
        def apply(l : H :: T, in : In) : Out = ft(l.tail, f(in, l.head))
      }
}

/**
 * Type class supporting right-folding a polymorphic binary function over this `HList`.
 * 
 * @author Miles Sabin
 */
trait RightFolder[L <: HList, In, HF] {
  type Out
  def apply(l : L, in : In) : Out 
}

trait RightFolderAux[L <: HList, In, HF, Out] {
  def apply(l : L, in : In) : Out 
}

object RightFolder {
  implicit def rightFolder[L <: HList, In, HF, Out0](implicit folder : RightFolderAux[L, In, HF, Out0]) =
    new RightFolder[L, In, HF] {
      type Out = Out0
      def apply(l : L, in : In) : Out = folder.apply(l, in)
    }
}

object RightFolderAux {
  import Poly._
  
  implicit def hnilRightFolderAux[In, HF] = new RightFolderAux[HNil, In, HF, In] {
    def apply(l : HNil, in : In) : In = in 
  }
  
  implicit def hlistRightFolderAux[H, T <: HList, In, HF, OutT, Out]
    (implicit ft : RightFolderAux[T, In, HF, OutT], f : Pullback2Aux[HF, H, OutT, Out]) =
      new RightFolderAux[H :: T, In, HF, Out] {
        def apply(l : H :: T, in : In) : Out = f(l.head, ft(l.tail, in))
      }
}

/**
 * Type class supporting left-reducing a polymorphic binary function over this `HList`.
 * 
 * @author Miles Sabin
 */
trait LeftReducer[L <: HList, HF] {
  type Out
  def apply(l : L) : Out 
}

object LeftReducer {
  implicit def leftReducer[H, T <: HList, HF](implicit folder : LeftFolder[T, H, HF]) =
    new LeftReducer[H :: T, HF] {
      type Out = folder.Out
      def apply(l : H :: T) : Out = folder.apply(l.tail, l.head)
    }
}

/**
 * Type class supporting right-reducing a polymorphic binary function over this `HList`.
 * 
 * @author Miles Sabin
 */
trait RightReducer[L <: HList, HF] {
  type Out
  def apply(l : L) : Out 
}

trait RightReducerAux[L <: HList, HF, Out] {
  def apply(l : L) : Out 
}

object RightReducer {
  implicit def rightReducer[L <: HList, HF, Out0](implicit reducer : RightReducerAux[L, HF, Out0]) =
    new RightReducer[L, HF] {
      type Out = Out0
      def apply(l : L) : Out = reducer.apply(l)
    }
}

object RightReducerAux {
  import Poly._
  
  implicit def hsingleRightReducerAux[H, HF] = new RightReducerAux[H :: HNil, HF, H] {
    def apply(l : H :: HNil) : H = l.head
  }
  
  implicit def hlistRightReducerAux[H, T <: HList, HF, OutT, Out]
    (implicit rt : RightReducerAux[T, HF, OutT], f : Pullback2Aux[HF, H, OutT, Out]) =
      new RightReducerAux[H :: T, HF, Out] {
        def apply(l : H :: T) : Out = f(l.head, rt(l.tail))
      }
}

/**
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common
 * supertype. 
 * 
 * @author Miles Sabin
 */
trait Lub[-A, -B, +Out] {
  def left(a : A) : Out
  def right(b : B) : Out
}

object Lub {
  implicit def lub[T] = new Lub[T, T, T] {
    def left(a : T) : T = a
    def right(b : T) : T = b
  }
}

/**
 * Type class supporting unification of this `HList`. 
 * 
 * @author Miles Sabin
 */
trait Unifier[L <: HList] {
  type Out
  def apply(l : L) : Out
}

trait UnifierAux[L <: HList, Out <: HList] {
  def apply(l : L) : Out
}
  
object Unifier {
  implicit def unifier[L <: HList, Out0 <: HList](implicit unifier : UnifierAux[L, Out0]) = new Unifier[L] {
    type Out = Out0
    def apply(l : L) : Out = unifier(l)
  }
}

object UnifierAux {
  implicit def hnilUnifier[T] = new UnifierAux[HNil, HNil] {
    def apply(l : HNil) = l
  }
  
  implicit def hsingleUnifier[T] = new UnifierAux[T :: HNil, T :: HNil] {
    def apply(l : T :: HNil) = l
  }
  
  implicit def hlistUnifier[H1, H2, L, T <: HList, Out <: HList]
    (implicit u : Lub[H1, H2, L], lt : UnifierAux[L :: T, L :: Out]) = new UnifierAux[H1 :: H2 :: T, L :: L :: Out] {
      def apply(l : H1 :: H2 :: T) : L :: L :: Out = u.left(l.head) :: lt(u.right(l.tail.head) :: l.tail.tail)
    }
}

/**
 * Type class supporting unification of all elements that are subtypes of `B` in this `HList` to `B`, with all other
 * elements left unchanged.
 * 
 * @author Travis Brown
 */
trait SubtypeUnifier[L <: HList, B] {
  type Out
  def apply(l : L) : Out
}

trait SubtypeUnifierAux[L <: HList, B, Out <: HList] {
  def apply(l : L) : Out
}
  
object SubtypeUnifier {
  implicit def subtypeUnifier[L <: HList, B, Out0 <: HList](implicit subtypeUnifier : SubtypeUnifierAux[L, B, Out0]) =
    new SubtypeUnifier[L, B] {
      type Out = Out0
      def apply(l : L) : Out = subtypeUnifier(l)
    }
}

object SubtypeUnifierAux {
  implicit def hnilSubtypeUnifier[B] = new SubtypeUnifierAux[HNil, B, HNil] {
    def apply(l : HNil) = l
  }
  
  implicit def hlistSubtypeUnifier1[H, T <: HList, B, NT <: HList]
    (implicit st : H <:< B, subtypeUnifier : SubtypeUnifierAux[T, B, NT]) = new SubtypeUnifierAux[H :: T, B, B :: NT] {
      def apply(l : H :: T) = st(l.head) :: subtypeUnifier(l.tail) 
    }
  
  implicit def hlistSubtypeUnifier2[H, T <: HList, B, NT <: HList]
    (implicit nst : H <:!< B, subtypeUnifier : SubtypeUnifierAux[T, B, NT]) =
      new SubtypeUnifierAux[H :: T, B, H :: NT] {
        def apply(l : H :: T) = l.head :: subtypeUnifier(l.tail) 
      }
}

/**
 * Type class supporting conversion of this `HList` to an ordinary `List` with elements typed as the least upper bound
 * of the types of the elements of this `HList`.
 * 
 * @author Miles Sabin
 */
trait ToList[-L <: HList, Lub] {
  def apply(l : L) : List[Lub]
}
  
object ToList {
  implicit def hnilToList[T] : ToList[HNil, T] = new ToList[HNil, T] {
    def apply(l : HNil) = Nil
  }
  
  implicit def hsingleToList[T] : ToList[T :: HNil, T] = new ToList[T :: HNil, T] {
    def apply(l : T :: HNil) = List(l.head)
  }
  
  implicit def hlistToList[H1, H2, T <: HList, L](implicit u : Lub[H1, H2, L], ttl : ToList[H2 :: T, L]) =
    new ToList[H1 :: H2 :: T, L] {
      def apply(l : H1 :: H2 :: T) = u.left(l.head) :: ttl(l.tail)
    }
}

/**
 * Type class supporting conversion of this `HList` to an `Array` with elements typed as the least upper bound
 * of the types of the elements of this `HList`.
 * 
 * @author Miles Sabin
 */
trait ToArray[-L <: HList, Lub] {
  def apply(n : Int, l : L) : Array[Lub]
}

object ToArray {
  import scala.reflect._ // Wildcard import for 2.9.x compatibility

  implicit def hnilToArray[T : ClassTag] : ToArray[HNil, T] = new ToArray[HNil, T] {
    def apply(n : Int, l : HNil) = Array.ofDim[T](n)
  }
  
  implicit def hsingleToArray[T : ClassTag] : ToArray[T :: HNil, T] = new ToArray[T :: HNil, T] {
    def apply(n : Int, l : T :: HNil) = {
      val arr = Array.ofDim[T](n+1)
      arr(n) = l.head
      arr
    }
  }
  
  implicit def hlistToArray[H1, H2, T <: HList, L](implicit u : Lub[H1, H2, L], tta : ToArray[H2 :: T, L]) =
    new ToArray[H1 :: H2 :: T, L] {
      def apply(n : Int, l : H1 :: H2 :: T) = {
        val arr = tta(n+1, l.tail)
        arr(n) = u.left(l.head)
        arr
      }
    }
}

/**
 * Type class supporting conversion of this `HList` to a tuple. 
 * 
 * @author Miles Sabin
 */
trait Tupler[L <: HList] {
  type Out <: Product
  def apply(l : L) : Out
}
  
trait TuplerAux[L <: HList, Out <: Product] {
  def apply(l : L) : Out
}
  
object Tupler {
  implicit def tupler[L <: HList, Out0 <: Product](implicit tupler : TuplerAux[L, Out0]) = new Tupler[L] {
    type Out = Out0
    def apply(l : L) : Out = tupler(l)
  }
}

object TuplerAux extends TuplerAuxInstances

/**
 * Type class supporting access to the last element of this `HList`. Available only if this `HList` has at least one
 * element.
 * 
 * @author Miles Sabin
 */
trait Last[L <: HList] {
  type Out
  def apply(l : L) : Out
}

trait LastAux[L <: HList, Out] {
  def apply(l : L) : Out
}
  
object Last {
  implicit def last[L <: HList, Out0](implicit last : LastAux[L, Out0]) = new Last[L] {
    type Out = Out0
    def apply(l : L) : Out = last(l)
  }
}

object LastAux {
  implicit def hsingleLast[H] = new LastAux[H :: HNil, H] {
    def apply(l : H :: HNil) : H = l.head
  }
  
  implicit def hlistLast[H, T <: HList, Out](implicit lt : LastAux[T, Out]) = new LastAux[H :: T, Out] {
    def apply(l : H :: T) : Out = lt(l.tail) 
  }
}

/**
 * Type class supporting access to all but the last element of this `HList`. Available only if this `HList` has at
 * least one element.
 * 
 * @author Miles Sabin
 */
trait Init[L <: HList] {
  type Out <: HList
  def apply(l : L) : Out
}

trait InitAux[L <: HList, Out <: HList] {
  def apply(l : L) : Out
}
  
object Init {
  implicit def init[L <: HList, Out0 <: HList](implicit init : InitAux[L, Out0]) = new Init[L] {
    type Out = Out0
    def apply(l : L) : Out = init(l)
  }
}

object InitAux {
  implicit def hsingleInit[H] = new InitAux[H :: HNil, HNil] {
    def apply(l : H :: HNil) : HNil = HNil
  }
  
  implicit def hlistInit[H, T <: HList, OutH, OutT <: HList](implicit it : InitAux[T, OutT]) =
    new InitAux[H :: T, H :: OutT] {
      def apply(l : H :: T) : H :: OutT = l.head :: it(l.tail)
    }
}  

/**
 * Type class supporting access to the first element of this `HList` of type `U`. Available only if this `HList`
 * contains an element of type `U`.
 * 
 * @author Miles Sabin
 */
trait Selector[L <: HList, U] {
  def apply(l : L) : U
}

object Selector {
  implicit def hlistSelect1[H, T <: HList] = new Selector[H :: T, H] {
    def apply(l : H :: T) = l.head
  }

  implicit def hlistSelect[H, T <: HList, U](implicit st : Selector[T, U]) = new Selector[H :: T, U] {
    def apply(l : H :: T) = st(l.tail)
  }
}

/**
 * Type class supporting access to the all elements of this `HList` of type `U`.
 * 
 * @author Alois Cochard
 */
trait Filter[L <: HList, U] {
  type Out <: HList
  def apply(l : L) : Out
}

object Filter {
  implicit def hlistFilter[L <: HList, U, Out0 <: HList](implicit aux : FilterAux[L, U, Out0]) = new Filter[L, U] {
    type Out = Out0
    def apply(l : L) : Out0 = aux(l)
  }
}

trait FilterAux[L <: HList, U, Out <: HList] {
  def apply(l : L) : Out
}

object FilterAux {
  implicit def hlistFilterHNil[L <: HList, U] = new FilterAux[HNil, U, HNil] {
     def apply(l : HNil) : HNil = HNil
  }

  implicit def hlistFilter1[L <: HList, H, Out <: HList]
    (implicit aux : FilterAux[L, H, Out]) = new FilterAux[H :: L, H, H :: Out] {
       def apply(l : H :: L) : H :: Out = l.head :: aux(l.tail)
    }

  implicit def hlistFilter2[H, L <: HList, U, Out <: HList]
    (implicit aux : FilterAux[L, U, Out], e : U =:!= H) = new FilterAux[H :: L, U, Out] {
       def apply(l : H :: L) : Out = aux(l.tail)
    }
}

/**
 * Type class supporting access to the all elements of this `HList` of type different than `U`.
 * 
 * @author Alois Cochard
 */
trait FilterNot[L <: HList, U] {
  type Out <: HList
  def apply(l : L) : Out
}

object FilterNot {
  implicit def hlistFilterNot[L <: HList, U, Out0 <: HList](implicit aux : FilterNotAux[L, U, Out0]) = new FilterNot[L, U] {
    type Out = Out0
    def apply(l : L) : Out0 = aux(l)
  }
}

trait FilterNotAux[L <: HList, U, Out <: HList] {
  def apply(l : L) : Out
}

object FilterNotAux {
  implicit def hlistFilterNotHNil[L <: HList, U] = new FilterNotAux[HNil, U, HNil] {
     def apply(l : HNil) : HNil = HNil
  }

  implicit def hlistFilterNot1[L <: HList, H, Out <: HList]
    (implicit aux : FilterNotAux[L, H, Out]) = new FilterNotAux[H :: L, H, Out] {
       def apply(l : H :: L) : Out = aux(l.tail)
    }

  implicit def hlistFilterNot2[H, L <: HList, U, Out <: HList]
    (implicit aux : FilterNotAux[L, U, Out], e: U =:!= H) = new FilterNotAux[H :: L, U, H :: Out] {
       def apply(l : H :: L) : H :: Out = l.head :: aux(l.tail)
    }
}

/**
 * Type class supporting removal of an element from this `HList`. Available only if this `HList` contains an
 * element of type `U`.
 * 
 * @author Stacy Curl
 */
trait Remove[E, L <: HList] {
  type Out <: HList
  def apply(l : L) : (E, Out)
}

trait RemoveAux[L <: HList, E, Rem <: HList] {
  def apply(l : L) : (E, Rem)
}

object Remove {
  implicit def hlistRemove[L <: HList, E, Rem <: HList](implicit aux: RemoveAux[L, E, Rem]) = new Remove[E, L] {
    type Out = Rem
    def apply(l : L) : (E, Rem) = aux(l)
  }
}

object RemoveAux {
  implicit def hlistRemove1[H, T <: HList] = new RemoveAux[H :: T, H, T] {
    def apply(l : H :: T) : (H, T) = (l.head, l.tail)
  }
  
  implicit def hlistRemove[H, T <: HList, E, Rem <: HList](implicit r : RemoveAux[T, E, Rem]) =
    new RemoveAux[H :: T, E, H :: Rem] {
      def apply(l : H :: T) : (E, H :: Rem) = {
        val (e, tail) = r(l.tail)
        (e, l.head :: tail)
      }
    }
}

/**
 * Type class supporting removal of a sublist from this `HList`. Available only if this `HLists contains a
 * sublist of type `SL`.
 *
 * The elements of `SL` do not have to be contiguous in this `HList`.
 * 
 * @author Stacy Curl
 */
trait RemoveAll[SL <: HList, L <: HList] {
  type Out <: HList
  def apply(l : L): (SL, Out)
}

trait RemoveAllAux[SL <: HList, L <: HList, Rem <: HList] {
  def apply(l : L): (SL, Rem)
}

object RemoveAll {
  implicit def removeAll[SL <: HList, L <: HList, Rem <: HList](implicit removeAll : RemoveAllAux[SL, L, Rem]) =
    new RemoveAll[SL, L] {
      type Out = Rem
      def apply(l : L) = removeAll(l)
    }
}

object RemoveAllAux {
  implicit def hlistRemoveAllSingle[L <: HList, E, Rem <: HList](implicit rt : RemoveAux[L, E, Rem]) =
    new RemoveAllAux[E :: HNil, L, Rem] {
      def apply(l : L): (E :: HNil, Rem) = {
        val (e, sub) = rt(l) 
        (e :: HNil, sub)
      }
    }

  implicit def hlistRemoveAll[L <: HList, E, RemE <: HList, Rem <: HList, SLT <: HList]
    (implicit rt : RemoveAux[L, E, RemE], st : RemoveAllAux[SLT, RemE, Rem]) = 
      new RemoveAllAux[E :: SLT, L, Rem] {
        def apply(l : L) : (E :: SLT, Rem) = {
          val (e, rem) = rt(l)
          val (sl, left) = st(rem)
          (e :: sl, left)
        }
      }
}

/**
 * Type class supporting replacement of the first element of type U from this `HList` with an element of type V.
 * Available only if this `HList` contains an element of type `U`.
 * 
 * @author Miles Sabin
 */
trait Replacer[L <: HList, U, V] {
  type Out <: HList
  def apply(l : L, v : V) : (U, Out)
}

trait ReplacerAux[L <: HList, U, V, Out <: HList] {
  def apply(l : L, v : V) : (U, Out)
}

object Replacer {
  implicit def replacer[L <: HList, U, V, Out0 <: HList](implicit replacer : ReplacerAux[L, U, V, Out0]) =
    new Replacer[L, U, V] {
      type Out = Out0
      def apply(l : L, v : V) : (U, Out) = replacer(l, v)
    }
}

object ReplacerAux {
  implicit def hlistReplacer1[T <: HList, U, V] = new ReplacerAux[U :: T, U, V, V :: T] {
    def apply(l : U :: T, v : V) : (U, V :: T) = (l.head, v :: l.tail)
  }
  
  implicit def hlistReplacer2[H, T <: HList, U, V, Out <: HList](implicit ut : ReplacerAux[T, U, V, Out]) =
    new ReplacerAux[H :: T, U, V, H :: Out] {
      def apply(l : H :: T, v : V) : (U, H :: Out) = {
        val (u, outT) = ut(l.tail, v)
        (u, l.head :: outT)
      }
    }
}

/**
 * Type class supporting replacement of the Nth element of this `HList` with an element of type V. Available only if
 * this `HList` contains at least N elements.
 * 
 * @author Miles Sabin
 */
trait ReplaceAt[L <: HList, N <: Nat, V] {
  type Elem
  type Out <: HList
  def apply(l : L, v : V) : (Elem, Out)
}

trait ReplaceAtAux[L <: HList, N <: Nat, U, V, Out <: HList] {
  def apply(l : L, v : V) : (U, Out)
}

object ReplaceAt {
  implicit def replaceAt[L <: HList, N <: Nat, Elem0, V, Out0 <: HList](implicit replacer : ReplaceAtAux[L, N, Elem0, V, Out0]) =
    new ReplaceAt[L, N, V] {
      type Elem = Elem0
      type Out = Out0
      def apply(l : L, v : V) : (Elem, Out) = replacer(l, v)
    }
}

object ReplaceAtAux {
  import Nat._
  
  implicit def hlistReplaceAt1[H, T <: HList, V] = new ReplaceAtAux[H :: T, _0, H, V, V :: T] {
    def apply(l : H :: T, v : V) : (H, V :: T) = (l.head, v :: l.tail)
  }
  
  implicit def hlistReplaceAt2[H, T <: HList, N <: Nat, U, V, Out <: HList](implicit ut : ReplaceAtAux[T, N, U, V, Out]) =
    new ReplaceAtAux[H :: T, Succ[N], U, V, H :: Out] {
      def apply(l : H :: T, v : V) : (U, H :: Out) = {
        val (u, outT) = ut(l.tail, v)
        (u, l.head :: outT)
      }
    }
}

/**
 * Type class supporting access to the ''nth'' element of this `HList`. Available only if this `HList` has at least
 * ''n'' elements. 
 * 
 * @author Miles Sabin
 */
trait At[L <: HList, N <: Nat] {
  type Out
  def apply(l : L) : Out 
}

trait AtAux[L <: HList, N <: Nat, Out] {
  def apply(l : L) : Out
}
  
object At {
  implicit def at[L <: HList, N <: Nat, Out0](implicit at : AtAux[L, N, Out0]) = new At[L, N] {
    type Out = Out0
    def apply(l : L) : Out = at(l)
  }
}

object AtAux {
  import Nat._
  
  implicit def hlistAtZero[H, T <: HList] = new AtAux[H :: T, _0, H] {
    def apply(l : H :: T) : H = l.head
  }
  
  implicit def hlistAtN[H, T <: HList, N <: Nat, Out](implicit att : AtAux[T, N, Out]) =
    new AtAux[H :: T, Succ[N], Out] {
      def apply(l : H :: T) : Out = att(l.tail) 
    }
}

/**
 * Type class supporting removal of the first ''n'' elements of this `HList`. Available only if this `HList` has at
 * least ''n'' elements.
 * 
 * @author Miles Sabin
 */
trait Drop[L <: HList, N <: Nat] {
  type Out <: HList
  def apply(l : L) : Out
}

trait DropAux[L <: HList, N <: Nat, Out <: HList] {
  def apply(l : L) : Out
}
  
object Drop {
  implicit def drop[L <: HList, N <: Nat, Out0 <: HList]
    (implicit drop : DropAux[L, N, Out0]) = new Drop[L, N] {
    type Out = Out0
    def apply(l : L) : Out = drop(l)
  }
}

object DropAux {
  import Nat._
  
  implicit def hlistDrop1[L <: HList] = new DropAux[L, _0, L] {
    def apply(l : L) : L = l
  }
  
  implicit def hlistDrop2[H, T <: HList, N <: Nat, Out <: HList](implicit dt : DropAux[T, N, Out]) =
    new DropAux[H :: T, Succ[N], Out] {
      def apply(l : H :: T) : Out = dt(l.tail)
    }
}

/**
 * Type class supporting retrieval of the first ''n'' elements of this `HList`. Available only if this `HList` has at
 * least ''n'' elements.
 * 
 * @author Miles Sabin
 */
trait Take[L <: HList, N <: Nat] {
  type Out <: HList
  def apply(l : L) : Out
}
  
trait TakeAux[L <: HList, N <: Nat, Out <: HList] {
  def apply(l : L) : Out
}

object Take {
  implicit def take[L <: HList, N <: Nat, Out0 <: HList]
    (implicit take : TakeAux[L, N, Out0]) = new Take[L, N] {
    type Out = Out0
    def apply(l : L) : Out = take(l)
  }
}  

object TakeAux {
  import Nat._

  implicit def hlistTake1[L <: HList] = new TakeAux[L, _0, HNil] {
    def apply(l : L) : HNil = HNil
  }
  
  implicit def hlistTake2[H, T <: HList, N <: Nat, Out <: HList](implicit tt : TakeAux[T, N, Out]) =
    new TakeAux[H :: T, Succ[N], H :: Out] {
      def apply(l : H :: T) : H :: Out = l.head :: tt(l.tail)
    }
}

/**
 * Type class supporting splitting this `HList` at the ''nth'' element returning the prefix and suffix as a pair.
 * Available only if this `HList` has at least ''n'' elements.
 * 
 * @author Miles Sabin
 */
trait Split[L <: HList, N <: Nat] {
  type Out = (P, S)
  type P <: HList
  type S <: HList
  def apply(l : L) : Out
} 
  
trait Split0[AccP <: HList, AccS <: HList, N <: Nat, P <: HList, S <: HList] {
  def apply(accP : AccP, accS : AccS) : (P, S)
}

object Split {
  implicit def split[L <: HList, N <: Nat, P0 <: HList, S0 <: HList]
    (implicit split : Split0[HNil, L, N, P0, S0]) = new Split[L, N] {
    type P = P0
    type S = S0
    def apply(l : L) : Out = split(HNil, l)
  }
}

object Split0 {
  import Nat._
  
  implicit def hlistSplit1[P <: HList, S <: HList] = new Split0[P, S, _0, P, S] {
    def apply(accP : P, accS : S) : (P, S) = (accP, accS)
  }

  implicit def hlistSplit2[AccP <: HList, AccSH, AccST <: HList, N <: Nat, P <: HList, S <: HList]
    (implicit st : Split0[AccP, AccST, N, P, S]) = new Split0[AccP, AccSH :: AccST, Succ[N], AccSH :: P, S] {
    def apply(accP : AccP, accS : AccSH :: AccST) : (AccSH :: P, S) =
      st(accP, accS.tail) match { case (prefix, suffix) => (accS.head :: prefix, suffix) }
  }
}

/**
 * Type class supporting splitting this `HList` at the ''nth'' element returning the reverse prefix and suffix as a
 * pair. Available only if this `HList` has at least ''n'' elements.
 * 
 * @author Miles Sabin
 */
trait ReverseSplit[L <: HList, N <: Nat] {
  type Out = (P, S)
  type P <: HList
  type S <: HList
  def apply(l : L) : Out
} 

trait ReverseSplit0[AccP <: HList, AccS <: HList, N <: Nat, P, S] {
  def apply(accP : AccP, accS : AccS) : (P, S)
}

object ReverseSplit {
  implicit def reverseSplit[L <: HList, N <: Nat, P0 <: HList, S0 <: HList]
    (implicit split : ReverseSplit0[HNil, L, N, P0, S0]) = new ReverseSplit[L, N] {
    type P = P0
    type S = S0
    def apply(l : L) : Out = split(HNil, l)
  }
}

object ReverseSplit0 {
  import Nat._
  
  implicit def hlistReverseSplit1[P <: HList, S <: HList] = new ReverseSplit0[P, S, _0, P, S] {
    def apply(accP : P, accS : S) : (P, S) = (accP, accS)
  }
  
  implicit def hlistReverseSplit2[AccP <: HList, AccSH, AccST <: HList, N <: Nat, P, S]
    (implicit st : ReverseSplit0[AccSH :: AccP, AccST, N, P, S]) =
      new ReverseSplit0[AccP, AccSH :: AccST, Succ[N], P, S] {
        def apply(accP : AccP, accS : AccSH :: AccST) : (P, S) = st(accS.head :: accP, accS.tail)
      }
}

/**
 * Type class supporting splitting this `HList` at the first occurence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 * 
 * @author Miles Sabin
 */
trait SplitLeft[L <: HList, U] {
  type Out = (P, S)
  type P <: HList
  type S <: HList
  def apply(l : L) : Out
} 
  
trait SplitLeft0[AccP <: HList, AccS <: HList, U, P <: HList, S <: HList] {
  def apply(accP : AccP, accS : AccS) : (P, S)
}

object SplitLeft {
  implicit def splitLeft[L <: HList, U, P0 <: HList, S0 <: HList]
    (implicit splitLeft : SplitLeft0[HNil, L, U, P0, S0]) = new SplitLeft[L, U] {
    type P = P0
    type S = S0
    def apply(l : L) : Out = splitLeft(HNil, l)
  }
}

trait LowPrioritySplitLeft0 {
  implicit def hlistSplitLeft1[AccP <: HList, AccSH, AccST <: HList, U, P <: HList, S <: HList]
    (implicit slt : SplitLeft0[AccP, AccST, U, P, S]) = new SplitLeft0[AccP, AccSH :: AccST, U, AccSH :: P, S] {
    def apply(accP : AccP, accS : AccSH :: AccST) : (AccSH :: P, S) =
      slt(accP, accS.tail) match { case (prefix, suffix) => (accS.head :: prefix, suffix) }
  }
}

object SplitLeft0 extends LowPrioritySplitLeft0 {
  implicit def hlistSplitLeft2[P <: HList, SH, ST <: HList] =
    new SplitLeft0[P, SH :: ST, SH, P, SH :: ST] {
      def apply(accP : P, accS : SH :: ST) : (P, SH :: ST) = (accP, accS)
    }
}

/**
 * Type class supporting splitting this `HList` at the first occurence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 * 
 * @author Miles Sabin
 */
trait ReverseSplitLeft[L <: HList, U] {
  type Out = (P, S)
  type P <: HList
  type S <: HList
  def apply(l : L) : Out
} 
  
trait ReverseSplitLeft0[AccP <: HList, AccS <: HList, U, P, S] {
  def apply(accP : AccP, accS : AccS) : (P, S)
}

object ReverseSplitLeft {
  implicit def reverseSplitLeft[L <: HList, U, P0 <: HList, S0 <: HList]
    (implicit splitLeft : ReverseSplitLeft0[HNil, L, U, P0, S0]) = new ReverseSplitLeft[L, U] {
    type P = P0
    type S = S0
    def apply(l : L) : Out = splitLeft(HNil, l)
  }
}

trait LowPriorityReverseSplitLeft0 {
  implicit def hlistReverseSplitLeft1[AccP <: HList, AccSH, AccST <: HList, U, P, S]
    (implicit slt : ReverseSplitLeft0[AccSH :: AccP, AccST, U, P, S]) =
      new ReverseSplitLeft0[AccP, AccSH :: AccST, U, P, S] {
        def apply(accP : AccP, accS : AccSH :: AccST) : (P, S) = slt(accS.head :: accP, accS.tail)
      }
}

object ReverseSplitLeft0 extends LowPriorityReverseSplitLeft0 {
  implicit def hlistReverseSplitLeft2[P <: HList, SH, ST <: HList] =
    new ReverseSplitLeft0[P, SH :: ST, SH, P, SH :: ST] {
      def apply(accP : P, accS : SH :: ST) : (P, SH :: ST) = (accP, accS)
    }
}  

/**
 * Type class supporting splitting this `HList` at the last occurence of an element of type `U` returning the prefix
 * and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 * 
 * @author Miles Sabin
 */
trait SplitRight[L <: HList, U] {
  type Out = (P, S)
  type P <: HList
  type S <: HList
  def apply(l : L) : Out
} 
  
trait SplitRight0[Rev <: HList, AccP <: HList, AccS <: HList, U, P <: HList, S <: HList] {
  def apply(rev : Rev, accP : AccP, accS : AccS) : (P, S)
}

object SplitRight {
  implicit def splitRight[L <: HList, U, P0 <: HList, S0 <: HList]
    (implicit splitRight : SplitRight0[L, HNil, HNil, U, P0, S0]) = new SplitRight[L, U] {
    type P = P0
    type S = S0
    def apply(l : L) : Out = splitRight(l, HNil, HNil)
  }
}

trait LowPrioritySplitRight0 {
  implicit def hlistSplitRight1[RevH, RevT <: HList, AccP <: HList, U, P <: HList, S <: HList]
    (implicit srt : SplitRight0[RevT, RevH :: AccP, HNil, U, P, S]) =
      new SplitRight0[RevH :: RevT, AccP, HNil, U, P, S] {
        def apply(rev : RevH :: RevT, accP : AccP, accS : HNil) : (P, S) = srt(rev.tail, rev.head :: accP, accS)
      }

  implicit def hlistSplitRight2[AccPH, AccPT <: HList, AccS <: HList, U, P <: HList, S <: HList]
    (implicit srt : SplitRight0[HNil, AccPT, AccPH :: AccS, U, P, S]) =
      new SplitRight0[HNil, AccPH :: AccPT, AccS, U, P, S] {
        def apply(rev : HNil, accP : AccPH :: AccPT, accS : AccS) : (P, S) = srt(rev, accP.tail, accP.head :: accS)
      }
}

object SplitRight0 extends LowPrioritySplitRight0 {
  implicit def hlistSplitRight3[PH, PT <: HList, S <: HList](implicit reverse : Reverse[PH :: PT]) =
    new SplitRight0[HNil, PH :: PT, S, PH, reverse.Out, S] {
      def apply(rev : HNil, accP : PH :: PT, accS : S) : (reverse.Out, S) = (accP.reverse, accS)
    }
}

/**
 * Type class supporting splitting this `HList` at the last occurence of an element of type `U` returning the reverse
 * prefix and suffix as a pair. Available only if this `HList` contains an element of type `U`.
 * 
 * @author Miles Sabin
 */
trait ReverseSplitRight[L <: HList, U] {
  type Out = (P, S)
  type P <: HList
  type S <: HList
  def apply(l : L) : Out
} 
  
trait ReverseSplitRight0[Rev <: HList, AccP <: HList, AccS <: HList, U, P, S] {
  def apply(rev : Rev, accP : AccP, accS : AccS) : (P, S)
}

object ReverseSplitRight {
  implicit def reverseSplitRight[L <: HList, U, P0 <: HList, S0 <: HList]
    (implicit splitRight : ReverseSplitRight0[L, HNil, HNil, U, P0, S0]) = new ReverseSplitRight[L, U] {
    type P = P0
    type S = S0
    def apply(l : L) : Out = splitRight(l, HNil, HNil)
  }
}

trait LowPriorityReverseSplitRight0 {
  implicit def hlistReverseSplitRight1[RevH, RevT <: HList, AccP <: HList, U, P <: HList, S <: HList]
    (implicit srt : ReverseSplitRight0[RevT, RevH :: AccP, HNil, U, P, S]) =
      new ReverseSplitRight0[RevH :: RevT, AccP, HNil, U, P, S] {
        def apply(rev : RevH :: RevT, accP : AccP, accS : HNil) : (P, S) = srt(rev.tail, rev.head :: accP, accS)
      }
  
  implicit def hlistReverseSplitRight2[AccPH, AccPT <: HList, AccS <: HList, U, P <: HList, S <: HList]
    (implicit srt : ReverseSplitRight0[HNil, AccPT, AccPH :: AccS, U, P, S]) =
      new ReverseSplitRight0[HNil, AccPH :: AccPT, AccS, U, P, S] {
        def apply(rev : HNil, accP : AccPH :: AccPT, accS : AccS) : (P, S) = srt(rev, accP.tail, accP.head :: accS)
      }
}

object ReverseSplitRight0 extends LowPriorityReverseSplitRight0 {
  implicit def hlistReverseSplitRight3[PH, PT <: HList, S <: HList] =
    new ReverseSplitRight0[HNil, PH :: PT, S, PH, PH :: PT, S] {
      def apply(rev : HNil, accP : PH :: PT, accS : S) = (accP, accS)
    }
}

/**
 * Type class supporting reversing this `HList`.
 * 
 * @author Miles Sabin
 */
trait Reverse[L <: HList] {
  type Out <: HList
  def apply(l : L) : Out
}

trait Reverse0[Acc <: HList, L <: HList, Out <: HList] {
  def apply(acc : Acc, l : L) : Out
}

object Reverse {
  implicit def reverse[L <: HList, Out0 <: HList](implicit reverse : Reverse0[HNil, L, Out0]) = new Reverse[L] {
    type Out = Out0
    def apply(l : L) : Out = reverse(HNil, l)
  }
}

object Reverse0 {
  implicit def hnilReverse[Out <: HList] = new Reverse0[Out, HNil, Out] {
    def apply(acc : Out, l : HNil) : Out = acc
  }
  
  implicit def hlistReverse[Acc <: HList, InH, InT <: HList, Out <: HList]
    (implicit rt : Reverse0[InH :: Acc, InT, Out]) = new Reverse0[Acc, InH :: InT, Out] {
      def apply(acc : Acc, l : InH :: InT) : Out = rt(l.head :: acc, l.tail)
    }
}

/**
 * Type class supporting prepending to this `HList`.
 * 
 * @author Miles Sabin
 */
trait Prepend[P <: HList, S <: HList] {
  type Out <: HList
  def apply(prefix : P, suffix : S) : Out
}

trait PrependAux[P <: HList, S <: HList, Out <: HList] {
  def apply(prefix : P, suffix : S) : Out
}

trait LowPriorityPrepend {
  implicit def hnilPrepend1[P <: HNil, S <: HList] = new Prepend[P, S] {
    type Out = S
    def apply(prefix: P, suffix: S) = suffix
  }
}

object Prepend extends LowPriorityPrepend {
  implicit def hnilPrepend2[P <: HList, S <: HNil] = new Prepend[P, S] {
    type Out = P
    def apply(prefix: P, suffix: S) = prefix
  }

  implicit def prepend[P <: HList, S <: HList, Out0 <: HList](implicit prepend : PrependAux[P, S, Out0]) =
    new Prepend[P, S] {
      type Out = Out0
      def apply(prefix : P, suffix : S) : Out = prepend(prefix, suffix)
    }
}

object PrependAux {
  implicit def hnilPrepend[S <: HList] = new PrependAux[HNil, S, S] {
    def apply(prefix : HNil, suffix : S) : S = suffix 
  }
  
  implicit def hlistPrepend[PH, PT <: HList, S <: HList, OutT <: HList](implicit pt : PrependAux[PT, S, OutT]) =
    new PrependAux[PH :: PT, S, PH :: OutT] {
      def apply(prefix : PH :: PT, suffix : S) : PH :: OutT = prefix.head :: pt(prefix.tail, suffix)
    }
}

/**
 * Type class supporting reverse prepending to this `HList`.
 * 
 * @author Miles Sabin
 */
trait ReversePrepend[P <: HList, S <: HList] {
  type Out <: HList
  def apply(prefix : P, suffix : S) : Out
}

trait ReversePrependAux[P <: HList, S <: HList, Out <: HList] {
  def apply(prefix : P, suffix : S) : Out
}

trait LowPriorityReversePrepend {
  implicit def hnilReversePrepend1[P <: HNil, S <: HList] = new ReversePrepend[P, S] {
    type Out = S
    def apply(prefix: P, suffix: S) = suffix
  }
}
  
object ReversePrepend extends LowPriorityReversePrepend {
  implicit def hnilReversePrepend2[P <: HList, S <: HNil](implicit rv: Reverse[P]) = new ReversePrepend[P, S] {
    type Out = rv.Out
    def apply(prefix: P, suffix: S) = prefix.reverse
  }

  implicit def reversePrepend[P <: HList, S <: HList, Out0 <: HList]
    (implicit prepend : ReversePrependAux[P, S, Out0]) =
      new ReversePrepend[P, S] {
        type Out = Out0
        def apply(prefix : P, suffix : S) : Out = prepend(prefix, suffix)
      }
}

object ReversePrependAux {
  implicit def hnilReversePrepend[S <: HList] = new ReversePrependAux[HNil, S, S] {
    def apply(prefix : HNil, suffix : S) : S = suffix 
  }
  
  implicit def hlistReversePrepend[PH, PT <: HList, S <: HList, Out <: HList]
    (implicit rpt : ReversePrependAux[PT, PH :: S, Out]) = new ReversePrependAux[PH :: PT, S, Out] {
      def apply(prefix : PH :: PT, suffix : S) : Out = rpt(prefix.tail, prefix.head :: suffix)
    }
}  

/**
 * Type class supporting zipping this `HList` with an `HList` of `HLists` returning an `HList` of `HList`s with each
 * element of this `HList` prepended to the corresponding `HList` element of the argument `HList`.
 * 
 * @author Miles Sabin
 */
trait ZipOne[H <: HList, T <: HList] {
  type Out <: HList
  def apply(h : H, t : T) : Out
}

trait ZipOneAux[H <: HList, T <: HList, Out <: HList] {
  def apply(h : H, t : T) : Out
}

object ZipOne {
  implicit def zipOne[H <: HList, T <: HList, Out0 <: HList](implicit zipOne : ZipOneAux[H, T, Out0]) =
    new ZipOne[H, T] {
      type Out = Out0
      def apply(h : H, t : T) : Out = zipOne(h, t)
    }
}

object ZipOneAux {
  implicit def zipOne1[H <: HList] = new ZipOneAux[H, HNil, HNil] {
    def apply(h : H, t : HNil) : HNil = HNil 
  }
  
  implicit def zipOne2[T <: HList] = new ZipOneAux[HNil, T, HNil] {
    def apply(h : HNil, t : T) : HNil = HNil 
  }

  implicit def zipOne3[H, T <: HList] = new ZipOneAux[H :: HNil, T :: HNil, (H :: T) :: HNil] {
    def apply(h : H :: HNil, t : T :: HNil) : (H :: T) :: HNil = (h.head :: t.head) :: HNil 
  }
  
  implicit def zipOne4[HH, HT <: HList, TH <: HList, TT <: HList, OutT <: HList]
    (implicit zot : ZipOneAux[HT, TT, OutT]) = new ZipOneAux[HH :: HT, TH :: TT, (HH :: TH) :: OutT] {
      def apply(h : HH :: HT, t : TH :: TT) : (HH :: TH) :: OutT = (h.head :: t.head) :: zot(h.tail, t.tail)
    }
}

/**
 * Type class supporting transposing this `HList`.
 * 
 * @author Miles Sabin
 */
trait Transposer[L <: HList] {
  type Out <: HList
  def apply(l : L) : Out
}

trait TransposerAux[L <: HList, Out <: HList] {
  def apply(l : L) : Out
}

object Transposer {
  implicit def transposer[L <: HList, Out0 <: HList](implicit transposer : TransposerAux[L, Out0]) = new Transposer[L] {
    type Out = Out0
    def apply(l : L) : Out = transposer(l)
  }
}

object TransposerAux {
  implicit def hnilTransposer = new TransposerAux[HNil, HNil] {
    def apply(l : HNil) = l 
  }
  
  implicit def hlistTransposer1[H <: HList, MC <: HList, Out <: HList]
    (implicit mc : ConstMapperAux[HNil, H, MC], zo : ZipOneAux[H, MC, Out]) = new TransposerAux[H :: HNil, Out] {
      def apply(l : H :: HNil) : Out = zo(l.head, mc(HNil, l.head))
    }
  
  implicit def hlistTransposer2[H <: HList, T <: HList, OutT <: HList, Out <: HList]
    (implicit tt : TransposerAux[T, OutT], zo : ZipOneAux[H, OutT, Out]) = new TransposerAux[H :: T, Out] {
      def apply(l : H :: T) : Out = zo(l.head, tt(l.tail))
    }
}

/**
 * Type class supporting zipping this `HList` of `HList`s returning an `HList` of tuples.
 * 
 * @author Miles Sabin
 */
trait Zip[L <: HList] {
  type Out <: HList
  def apply(l : L) : Out
}

object Zip {
  import Tuples._
  
  implicit def zipper[L <: HList, OutT <: HList, OutM <: HList]
    (implicit
      transposer : TransposerAux[L, OutT],
      mapper : MapperAux[tupled.type, OutT, OutM]) = new Zip[L] {
    type Out = OutM
    def apply(l : L) = l.transpose map tupled
  }
}

/**
 * Type class supporting unzipping this `HList` of tuples returning a tuple of `HList`s.
 * 
 * @author Miles Sabin
 */
trait Unzip[L <: HList] {
  type Out <: Product
  def apply(l : L) : Out
}

object Unzip {
  import Tuples._

  implicit def unzipper[L <: HList, OutM <: HList, OutT <: HList]
    (implicit
      mapper : MapperAux[hlisted.type, L, OutM],
      transposer : TransposerAux[OutM, OutT],
      tupler : Tupler[OutT]) = new Unzip[L] {
    type Out = tupler.Out
    def apply(l : L) = (l map hlisted).transpose.tupled
  }
}
  
/**
 * Type class supporting zipping this this `HList` of monomorphic function values with its argument `HList` of
 * correspondingly typed function arguments returning the result of each application as an `HList`. Available only if
 * there is evidence that the corresponding function and argument elements have compatible types.
 * 
 * @author Miles Sabin
 */
trait ZipApply[FL <: HList, AL <: HList] {
  type Out <: HList
  def apply(fl : FL, al : AL) : Out
}

trait ZipApplyAux[FL <: HList, AL <: HList, Out <: HList] {
  def apply(fl : FL, al : AL) : Out
}

object ZipApply {
  implicit def zipApply[FL <: HList, AL <: HList, Out0 <: HList](implicit zap : ZipApplyAux[FL, AL, Out0]) =
    new ZipApply[FL, AL] {
      type Out = Out0
      def apply(fl : FL, al : AL) : Out = zap(fl, al)
    }
}

object ZipApplyAux {
  implicit def hnilZipApply = new ZipApplyAux[HNil, HNil, HNil] {
    def apply(fl : HNil, al : HNil) : HNil = HNil
  }
  
  implicit def hconsZipApply[T, R, FLT <: HList, ALT <: HList, OutT <: HList]
    (implicit ztt : ZipApplyAux[FLT, ALT, OutT]) = new ZipApplyAux[(T => R) :: FLT, T :: ALT, R :: OutT] {
      def apply(fl : (T => R) :: FLT, al : T :: ALT) : R :: OutT = fl.head(al.head) :: ztt(fl.tail, al.tail) 
    }
}
