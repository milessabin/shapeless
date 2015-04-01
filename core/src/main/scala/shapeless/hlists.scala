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

import scala.language.dynamics
import scala.language.experimental.macros

import scala.annotation.tailrec
import scala.reflect.macros.Context

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
  
  implicit def hlistOps[L <: HList](l : L) : HListOps[L] = new HListOps(l)

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
 */
trait ProductArgs extends Dynamic {
  def applyDynamic(method: String)(args: Any*): Any = macro ProductMacros.forwardImpl
}

object ProductMacros {
  def inst(c: Context) = new ProductMacros[c.type](c)

  def forwardImpl(c: Context)(method: c.Expr[String])(args: c.Expr[Any]*): c.Expr[Any] =
    c.Expr[Any](inst(c).forwardImpl(method.tree)(args.map(_.tree): _*))
}

class ProductMacros[C <: Context](val c: C) {
  import c.universe._

  val hconsValueTree = reify {  ::  }.tree
  val hnilValueTree  = reify { HNil: HNil }.tree

  def forwardImpl(method: Tree)(args: Tree*): Tree = {
    val lhs = c.prefix.tree 
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = method
    val methodName = newTermName(methodString+"Product")

    if(lhsTpe.member(methodName) == NoSymbol)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val argsTree = mkProductImpl(args: _*)

    q""" $lhs.$methodName($argsTree) """
  }

  def mkProductImpl(args: Tree*): Tree = {
    args.foldRight(hnilValueTree) {
      case(elem, acc) => q""" $hconsValueTree($elem, $acc) """
    }
  }
}
