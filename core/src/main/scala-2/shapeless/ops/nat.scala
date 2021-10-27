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

import scala.annotation.tailrec
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

trait ToIntScalaCompat {
  implicit def toIntSuccM[N <: Nat]: nat.ToInt[N] = macro ToIntMacros.applyImpl[N]
}
class ToIntMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  val _0Tpe = typeOf[_0]
  val succTpe = typeOf[Succ[_]].typeConstructor
  val succSym = succTpe.typeSymbol
  val succPre = prefix(succTpe)


  def applyImpl[N <: Nat](implicit nTag: WeakTypeTag[N]): Tree = {
    val tpe = nTag.tpe.dealias

    @tailrec
    def count(u: Type, acc: Int): Int = {
      if(u <:< _0Tpe) acc
      else (u baseType succSym) match {
        case TypeRef(pre, _, List(n)) if pre =:= succPre => count(n, acc + 1)
        case _ => abort(s"$tpe is not a Nat type")
      }
    }

    q"""
            new _root_.shapeless.ops.nat.ToInt.Inst(${count(tpe, 0)}).
              asInstanceOf[ _root_.shapeless.ops.nat.ToInt[$tpe]]
          """
  }
}
