/*
 * Copyright (c) 2018 Miles Sabin
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

import scala.collection.IterableOps
import scala.collection.generic.IsIterableLike
import scala.reflect.macros.whitebox

trait ScalaVersionSpecifics extends LP0 {
  private[shapeless] type IsRegularIterable[Repr] = collection.generic.IsIterableLike[Repr]

  private[shapeless] def implicitNotFoundMessage(c: whitebox.Context)(tpe: c.Type): String = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.typeSymbolDirect match {
      case global.analyzer.ImplicitNotFoundMsg(msg) =>
        msg.format(global.newTermName("evidence"), gTpe)
      case _ =>
        s"Implicit value of type $tpe not found"
    }
  }

  private[shapeless] object macrocompat {
    class bundle extends annotation.Annotation
  }

  private[shapeless] implicit class NewIsIterable0[A0, Repr](itl: IsIterableLike[Repr] { type A = A0 }) {
    def apply(r: Repr): IterableOps[A0, Iterable, Repr] = itl.conversion(r)
  }
}

trait LP0 {
  private[shapeless] implicit class NewIsIterable1[Repr](val itl: IsRegularIterable[Repr]) {
    def apply(r: Repr): IterableOps[_, Iterable, Repr] = itl.conversion(r)
  }
}

trait CaseClassMacrosVersionSpecifics { self: CaseClassMacros =>
  import c.universe._

  val varargTpt = tq"_root_.scala.collection.immutable.Seq"
  val varargTC = typeOf[scala.collection.immutable.Seq[_]].typeConstructor
}
