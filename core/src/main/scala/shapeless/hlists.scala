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

import scala.language.dynamics

import scala.annotation.tailrec

/**
 * `HList` ADT base trait.
 * 
 * @author Miles Sabin
 */
sealed trait HList extends Product with Serializable

/**
 * Non-empty `HList` element type.
 * 
 * @author Miles Sabin
 */
final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
  override def toString: String = head match {
    case _: ::[_, _] => s"($head) :: $tail"
    case _ => s"$head :: $tail"
  }
}

/**
 * Empty `HList` element type.
 * 
 * @author Miles Sabin
 */
sealed trait HNil extends HList {
  def ::[H](h: H): H :: HNil = new ::(h, this)
  override def toString = "HNil"
}

/**
 * Empty `HList` value.
 * 
 * @author Miles Sabin
 */
case object HNil extends HNil

object HList extends Dynamic with HListScalaCompat {
  import ops.hlist._
  import syntax.HListOps

  def apply(): HNil.type = HNil

  def apply[T](t: T): T :: HNil = t :: HNil

  def apply[P <: Product, L <: HList](p : P)(implicit gen: Generic.Aux[P, L]) : L = gen.to(p)

  /**
   * Produces a HList of length `N` filled with `elem`.
   */
  def fill[A](n: Nat)(elem: A)(implicit fill: Fill[n.N, A]) : fill.Out = fill(elem)

  /**
   * Produces a `N1`-length HList made of `N2`-length HLists filled with `elem`.
   */
  def fill[A](n1: Nat, n2: Nat)(elem: A)(implicit fill: Fill[(n1.N, n2.N), A]) : fill.Out = fill(elem)

  final class FillWithOps[L <: HList] {
    def apply[F <: Poly](f: F)(implicit fillWith: FillWith[F, L]): L = fillWith()
  }

  /**
    * Produces a [[HList]] filled from a [[Poly0]].
    * 
    * @example def fillWith[L <: HList](f: Poly): L = ???
    */
  def fillWith[L <: HList] = new FillWithOps[L]

  implicit def hlistOps[L <: HList](l: L): HListOps[L] = new HListOps(l)

  /**
   * Convenience aliases for HList :: and List :: allowing them to be used together within match expressions.  
   */
  object ListCompat {
    val :: = scala.collection.immutable.::
    val #: = shapeless.::
  }

  @tailrec
  def unsafeGet(l: HList, i: Int): Any =
    (l: @unchecked) match {
      case hd :: tl if i == 0 => hd
      case hd :: tl => unsafeGet(tl, i-1)
    }

  def unsafeReversePrepend(l: HList, m: HList): HList = {
    @tailrec
    def loop(l: HList, suffix: HList): HList =
      l match {
        case HNil => suffix
        case hd :: tl => loop(tl, hd :: suffix)
      }
    loop(l, m)
  }

  def unsafeReverse(l: HList): HList =
    unsafeReversePrepend(l, HNil)

  def unsafePrepend(l: HList, m: HList): HList =
    unsafeReversePrepend(unsafeReverse(l), m)

  def unsafeUpdateAt(l: HList, i: Int, e: Any): HList = {
    @tailrec
    def loop(l: HList, i: Int, revPrefix: HList): HList =
      (l: @unchecked) match {
        case hd :: tl if i == 0 => unsafeReversePrepend(revPrefix, e :: tl)
        case hd :: tl => loop(tl, i-1, hd :: revPrefix)
      }
    loop(l, i, HNil)
  }

  def unsafeUpdateAppend(l: HList, i: Int, e: Any): HList = {
    @tailrec
    def loop(l: HList, i: Int, revPrefix: HList): HList =
      l match {
        case HNil => unsafeReversePrepend(revPrefix, e :: HNil)
        case hd :: tl if i == 0 => unsafeReversePrepend(revPrefix, e :: tl)
        case hd :: tl => loop(tl, i-1, hd :: revPrefix)
      }
    loop(l, i, HNil)
  }

  def unsafeUpdateWith(l: HList, i: Int, f: Any => Any): HList = {
    @tailrec
    def loop(l: HList, i: Int, revPrefix: HList): HList =
      (l: @unchecked) match {
        case hd :: tl if i == 0 => unsafeReversePrepend(revPrefix, f(hd) :: tl)
        case hd :: tl => loop(tl, i-1, hd :: revPrefix)
      }
    loop(l, i, HNil)
  }

  def unsafeRemove(l: HList, i: Int): (Any, HList) = {
    @tailrec
    def loop(l: HList, i: Int, revPrefix: HList): (Any, HList) =
      (l: @unchecked) match {
        case hd :: tl if i == 0 => (hd, unsafeReversePrepend(revPrefix, tl))
        case hd :: tl => loop(tl, i-1, hd :: revPrefix)
      }
    loop(l, i, HNil)
  }
}

/**
 * Trait supporting mapping dynamic argument lists of Ints to HList of Nat arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(1, 2, 3)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(_1 :: _2 :: _3)
 * }}}
 *
 * ie. the arguments are rewritten as HList elements of Nat and the application is
 * rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList of Int argument.
 *
 * @author Andreas Koestler
 */

trait NatProductArgs extends Dynamic with NatProductArgsScalaCompat
/**
 * Trait supporting mapping dynamic argument lists to HList arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(23 :: "foo" :: true)
 * }}}
 *
 * ie. the arguments are rewritten as HList elements and the application is
 * rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList argument.
 *
 */
trait ProductArgs extends Dynamic with ProductArgsScalaCompat

/**
 * Trait supporting mapping HList arguments to argument lists, inverse of ProductArgs.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.methodProduct(23 :: "foo" :: true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * ie. the HList argument is used to obtain arguments for a target method
 * (the called method named minus the "Product" suffix) in sequence  and the application
 * is rewritten to an application of the target method
 *
 */
trait FromProductArgs extends Dynamic with FromProductArgsScalaCompat

/**
 * Trait supporting mapping dynamic argument lists to singleton-typed HList arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(23.narrow :: "foo".narrow :: true.narrow)
 * }}}
 *
 * ie. the arguments are rewritten as singleton-typed HList elements and the
 * application is rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList argument.
 */
trait SingletonProductArgs extends Dynamic with SingletonProductArgsScalaCompat
