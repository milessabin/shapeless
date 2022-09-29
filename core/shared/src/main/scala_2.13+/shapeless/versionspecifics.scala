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

import scala.reflect.macros.whitebox

trait LazyInstances {
  implicit def mkLazy[I](implicit i: => I): Lazy[I] = Lazy(i)
}

trait StrictInstances {
  implicit def mkStrict[I](implicit i: I): Strict[I] = Strict(i)
}

trait WitnessInstances {
  implicit def of[T: ValueOf]: Witness.Aux[T] =
    Witness.mkWitness(valueOf[T])

  implicit def apply[T](t: T): Witness.Aux[t.type] =
    Witness.mkWitness[t.type](t)
}

trait WitnessWithInstances {
  implicit def apply[TC[_], T](t: T)(implicit tc: TC[t.type]): WitnessWith.Aux[TC, t.type] { val instance: tc.type } =
    instance[TC, t.type](t, tc)

  def instance[TC[_], A](v: A, tc: TC[A]): WitnessWith.Aux[TC, A] { val instance: tc.type } =
    new WitnessWith[TC] {
      type T = A
      val value: T = v
      val instance: tc.type = tc
    }
}

trait ScalaVersionSpecifics {
  private[shapeless] type IsRegularIterable[Repr] = collection.generic.IsIterable[Repr] { type C = Repr }

  private[shapeless] def implicitNotFoundMessage(c: whitebox.Context)(tpe: c.Type): String = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.typeSymbolDirect match {
      case global.analyzer.ImplicitNotFoundMsg(msg) =>
        msg.formatDefSiteMessage(gTpe)
      case _ =>
        s"Implicit value of type $tpe not found"
    }
  }
}

trait CaseClassMacrosVersionSpecifics { self: CaseClassMacros =>
  import c.universe._

  val varargTpt = tq"_root_.scala.collection.immutable.Seq"
  val varargTC = typeOf[scala.collection.immutable.Seq[_]].typeConstructor
}
