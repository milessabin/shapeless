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

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait NatScalaCompat {
  implicit def apply(i: Int): Nat = macro NatMacros.materializeWidened
}

trait NatWithTypeAtPosScalaCompat {
  implicit def fromInt[L](i: Int): NatWithTypeAtPos[L] = macro NatMacros.makeNatWithTypeAtPos[L]
}

class NatMacros(val c: whitebox.Context) extends NatMacroDefns with CaseClassMacros {
  import c.universe._

  def materializeWidened(i: Tree): Tree =
    i match {
      case NatLiteral(n) => mkNatValue(n)
      case _ =>
        c.abort(c.enclosingPosition, s"Expression $i does not evaluate to a non-negative Int literal")
    }

  def makeNatWithTypeAtPos[L: WeakTypeTag](i: Tree): Tree = {
    i match {
      case NatLiteral(n) =>
        val L = weakTypeOf[L].dealias
        val N = mkNatTpt(n)

        val elements = if(L <:< hlistTpe) {
          unpackHList(L)
        } else if (isTuple(L)) {
          fieldsOf(L).map(_._2)
        } else {
          c.abort(c.enclosingPosition, s"The list $L must be either an HList or a tuple")
        }

        if (n >= elements.length) {
          c.abort(c.enclosingPosition, s"The list $L is too short to have an element at index $n")
        }

        val Out = elements(n)

        q"""new _root_.shapeless.NatWithTypeAtPos[$L] {
              type N = $N
              type Tpe = $Out
              val value: $N = ${mkNatValue(n)}
            }"""
      case _ =>
        c.abort(c.enclosingPosition, s"Expression $i does not evaluate to a non-negative Int literal")
    }
  }
}

trait NatMacroDefns {
  val c: whitebox.Context
  import c.universe._

  object NatLiteral {
    def unapply(i: Tree): Option[Int] =
      i match {
        case Literal(Constant(n: Int)) if n >= 0 => Some(n)
        case _ => None
      }
  }

  def mkNatTpt(i: Int): Tree = {
    val succSym = typeOf[Succ[_]].typeConstructor.typeSymbol
    val _0Sym = typeOf[_0].typeSymbol

    @tailrec
    def loop(i: Int, acc: Tree): Tree = {
      if(i == 0) acc
      else loop(i-1, AppliedTypeTree(Ident(succSym), List(acc)))
    }

    loop(i, Ident(_0Sym))
  }

  def mkNatTpe(i: Int): Type = {
    val succTpe = typeOf[Succ[_]].typeConstructor
    val _0Tpe = typeOf[_0]

    @tailrec
    def loop(i: Int, acc: Type): Type = {
      if(i == 0) acc
      else loop(i-1, appliedType(succTpe, acc))
    }

    loop(i, _0Tpe)
  }

  def mkNatValue(i: Int): Tree =
    q""" new ${mkNatTpt(i)} """
}
