/*
 * Copyright (c) 2016 Miles Sabin
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
import scala.language.reflectiveCalls

import scala.reflect.macros.Context

/*
 * Workaround for Scala 2.10 consle macro classloader bug. See,
 *
 *  https://github.com/milessabin/shapeless/issues/367
 *  https://github.com/milessabin/shapeless/issues/547
 *
 * See corresponding definition for 2.11+
 */
object LazyMacrosRef {
  def inst(c: Context) =
    new LazyMacros(new macrocompat.RuntimeCompatContext(c.asInstanceOf[scala.reflect.macros.runtime.Context]))

  def mkLazyImpl[I: c.WeakTypeTag](c: Context): c.Expr[Lazy[I]] = {
    import c.universe._

    def forward: c.Expr[Lazy[I]] = {
      val i = inst(c)
      val res = i.mkLazyImpl[I].asInstanceOf[c.Tree]
      c.Expr[Lazy[I]](res)
    }

    val lmSym = typeOf[LazyMacrosRef.type].typeSymbol
    lmSym.attachments.all.headOption match {
      case Some(lm) =>
        if(lm == LazyMacrosRef) forward
        else {
          lm.asInstanceOf[
            { def mkLazyImpl(c: Context)(i: c.WeakTypeTag[I]): c.Expr[Lazy[I]] }
          ].mkLazyImpl(c)(weakTypeTag[I])
        }
      case None =>
        lmSym.updateAttachment[LazyMacrosRef.type](this)
        try {
          forward
        } finally {
            lmSym.removeAttachment[LazyMacrosRef.type]
        }
    }
  }

  def mkStrictImpl[I: c.WeakTypeTag](c: Context): c.Expr[Strict[I]] = {
    import c.universe._

    def forward: c.Expr[Strict[I]] = {
      val i = inst(c)
      val res = i.mkStrictImpl[I].asInstanceOf[c.Tree]
      c.Expr[Strict[I]](res)
    }

    val lmSym = typeOf[LazyMacrosRef.type].typeSymbol
    lmSym.attachments.all.headOption match {
      case Some(lm) =>
        if(lm == LazyMacrosRef) forward
        else {
          lm.asInstanceOf[
            { def mkStrictImpl(c: Context)(i: c.WeakTypeTag[I]): c.Expr[Strict[I]] }
          ].mkStrictImpl(c)(weakTypeTag[I])
        }
      case None =>
        lmSym.updateAttachment[LazyMacrosRef.type](this)
        try {
          forward
        } finally {
            lmSym.removeAttachment[LazyMacrosRef.type]
        }
    }
  }
}
