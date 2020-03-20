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
import scala.language.experimental.macros

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

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
  override def toString = head match {
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

object HList extends Dynamic {
  import ops.hlist._
  import syntax.HListOps

  def apply() = HNil

  def apply[T](t: T) = t :: HNil

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

  /**
   * Allows to specify an `HList` type with a syntax similar to `Record` and `Union`, as follows,
   *
   * {{{
   * type ISB = HList.`Int, String, Boolean`.T
   * }}}
   *
   * Literal types are allowed, so that the following is valid,
   *
   * {{{
   * type ABC = HList.`'a, 'b, 'c`.T
   * type TwoTrueStr = HList.`2, true, "str"`.T
   * }}}
   */
  def selectDynamic(tpeSelector: String): Any = macro LabelledMacros.hlistTypeImpl

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

  @deprecated("use unsafeUpdateAppend instead", "2.3.1")
  def unsafeUpdate(l: HList, i: Int, e: Any): HList =
    unsafeUpdateAppend(l, i, e)

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

trait NatProductArgs extends Dynamic {
  def applyDynamic(method: String)(args: Int*): Any = macro ProductMacros.forwardNatImpl
}
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
trait ProductArgs extends Dynamic {
  def applyDynamic(method: String)(args: Any*): Any = macro ProductMacros.forwardImpl
}

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
trait FromProductArgs extends Dynamic {
  def applyDynamic[L <: HList](method: String)(product: L): Any =
    macro ProductMacros.forwardFromProductImpl[L]
}

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
trait SingletonProductArgs extends Dynamic {
  def applyDynamic(method: String)(args: Any*): Any = macro ProductMacros.forwardSingletonImpl
}

class ProductMacros(val c: whitebox.Context) extends SingletonTypeUtils with NatMacroDefns {
  import c.universe._

  def forwardImpl(method: Tree)(args: Tree*): Tree = forward(method, args, false)

  def forwardNatImpl(method: Tree)(args: Tree*): Tree = forwardNat(method, args)

  def forwardSingletonImpl(method: Tree)(args: Tree*): Tree = forward(method, args, true)

  def forwardNat(method: Tree, args: Seq[Tree]): Tree = {
    val lhs = c.prefix.tree
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = (method: @unchecked)
    val methodName = TermName(methodString+"NatProduct")

    if(lhsTpe.member(methodName) == NoSymbol)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val meth = lhsTpe.member(methodName).asMethod

    if (!meth.paramLists.isEmpty && (meth.paramLists(0) forall (_.isImplicit))) {
      val typeParamsTree = mkProductNatTypeParamsImpl(args)
      q""" $lhs.$methodName[${typeParamsTree}] """
    } else {
      val argsTree = mkProductNatImpl(args)
      q""" $lhs.$methodName($argsTree) """
    }
  }

  def forward(method: Tree, args: Seq[Tree], narrow: Boolean): Tree = {
    val lhs = c.prefix.tree 
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = (method: @unchecked)
    val methodName = TermName(methodString+"Product")

    if(lhsTpe.member(methodName) == NoSymbol)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val argsTree = mkProductImpl(args, narrow)

    q""" $lhs.$methodName($argsTree) """
  }

  def forwardFromProductImpl[L <: HList](method: Tree)(product: Expr[L]): Tree = {
    val lhs = c.prefix.tree
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = (method: @unchecked)

    if (!methodString.matches(".*Product$"))
      c.abort(c.enclosingPosition, s"missing method '$methodString'")

    val methodName = TermName(methodString.replaceAll("Product$", ""))

    if(!lhsTpe.member(methodName).isMethod)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val params = mkParamsImpl(lhsTpe.member(methodName).asMethod, product)
    q""" $lhs.$methodName(...$params) """
  }

  def mkProductImpl(args: Seq[Tree], narrow: Boolean): Tree = {
    args.foldRight((hnilTpe, q"_root_.shapeless.HNil: $hnilTpe": Tree)) {
      case(elem, (accTpe, accTree)) =>
        val (neTpe, neTree) = if(narrow) narrowValue(elem) else (elem.tpe, elem)
        (appliedType(hconsTpe, List(neTpe, accTpe)), q"""_root_.shapeless.::[$neTpe, $accTpe]($neTree, $accTree)""")
    }._2
  }

  def mkProductNatImpl(args: Seq[Tree]): Tree = {
    args.foldRight((tq"_root_.shapeless.HNil", q"_root_.shapeless.HNil: $hnilTpe"): (Tree, Tree)) {
      case(NatLiteral(n), (accTpt, accTree)) =>
        val neTpt = mkNatTpt(n)
        val neTree = mkNatValue(n)
        (tq"""_root_.shapeless.::[$neTpt, $accTpt]""", q"""_root_.shapeless.::[$neTpt, $accTpt]($neTree, $accTree)""")
      case (elem, _) =>
        c.abort(c.enclosingPosition, s"Expression $elem does not evaluate to a non-negative Int literal")
    }._2
  }

  def mkProductNatTypeParamsImpl(args: Seq[Tree]): Tree = {
    args.foldRight((tq"_root_.shapeless.HNil", tq"_root_.shapeless.HNil"): (Tree, Tree)) {
      case (NatLiteral(n), (accTpt, _)) =>
        val neTpt = mkNatTpt(n)
        (tq"""_root_.shapeless.::[$neTpt, $accTpt]""", tq"""_root_.shapeless.::[$neTpt, $accTpt]""")
       case (elem, _) =>
        c.abort(c.enclosingPosition, s"Expression $elem does not evaluate to a non-negative Int literal")
    }._2
  }

  def mkParamsImpl[L <: HList](method: MethodSymbol, product: Expr[L]): List[List[Tree]] = {
    val slices = method.paramLists.filterNot(_.forall(x => x.isImplicit))
      .foldLeft((List[List[Int]](), 0))((acc, e) =>
        (acc._1 :+ (acc._2 to (acc._2 + (e.size - 1))).toList, acc._2 + e.size))._1

    slices.map(_.map(i => q"${product}.apply(${i})"))
  }
}
