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

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait NatProductArgsScalaCompat {
  def applyDynamic(method: String)(args: Int*): Any = macro ProductMacros.forwardNatImpl
}

trait ProductArgsScalaCompat {
  def applyDynamic(method: String)(args: Any*): Any = macro ProductMacros.forwardImpl
}

trait FromProductArgsScalaCompat {
  def applyDynamic(method: String)(hlist: HList): Any = macro ProductMacros.forwardFromProductImpl
}

trait SingletonProductArgsScalaCompat {
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

  def forwardFromProductImpl(method: Tree)(hlist: Tree): Tree = {
    val lhs = c.prefix.tree
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = (method: @unchecked)

    if (!methodString.matches(".*Product$"))
      c.abort(c.enclosingPosition, s"missing method '$methodString'")

    val methodName = TermName(methodString.replaceAll("Product$", ""))

    if(!lhsTpe.member(methodName).isMethod)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val methodSym = lhsTpe.member(methodName).asMethod
    val paramss = methodSym.paramLists.filterNot(_.forall(_.isImplicit))
    val argss = paramss.map(_.map(_ => TermName(c.freshName("pat"))))
    val names = argss.flatten

    val pattern =
      names.foldRight(q"_root_.shapeless.HNil": Tree) {
        case (nme, acc) => pq"_root_.shapeless.::($nme, $acc)"
      }

    q"""
      $hlist match {
        case $pattern => $lhs.$methodName(...$argss)
      }
    """
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
}
