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

import shapeless.ops.hlist.ToSized

import scala.collection.{BuildFrom, Factory}
import scala.reflect.macros.whitebox

trait ScalaVersionSpecifics {
  private[shapeless] type IsRegularIterable[Repr] = collection.generic.IsIterable[Repr] { type C = Repr }
  private[shapeless] type TraversableOrIterable[+T] = Iterable[T]
  private[shapeless] type GenTraversableOrIterable[+T] = Iterable[T]

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

private[shapeless] trait AdditiveCollectionVersionSpecific

private[shapeless] trait SizedVersionSpecific {
  def apply[CC[_]]()(
    implicit dis: DefaultToIndexedSeq[CC],
    cbf: Factory[Nothing, CC[Nothing]],
    ev: AdditiveCollection[CC[Nothing]]
  ): Sized[CC[Nothing], _0] =
    Sized.wrap[CC[Nothing], _0](cbf.newBuilder.result())
}

private[shapeless] trait SizedOpsVersionSpecific[A0, Repr, L <: Nat] { self: SizedOps[A0, Repr, L] =>

  /**
   * Append the argument collection to this collection. The resulting collection will be statically known to have
   * ''m+n'' elements.
   */
  def ++[B >: A0, That, M <: Nat](that: Sized[That, M])(
    implicit sum: ops.nat.Sum[L, M],
    convThat: IsRegularIterable[That] { type A = B },
    cbf: Factory[B, That],
    ev: AdditiveCollection[That]
  ): Sized[That, sum.Out] =
    Sized.wrap[That, sum.Out](cbf.fromSpecific(underlying.iterator ++ convThat(that.unsized).iterator))
}

private[shapeless] trait RepeatVersionSpecific[L <: HList] extends DepFn1[L] with Serializable {
  type Out <: HList
}

private[shapeless] trait ToSizedVersionSpecific {
  implicit def hlistToSized[H1, H2, T <: HList, LT, L, N0 <: Nat, M[_]](
    implicit
    tts   : ToSized.Aux[H2 :: T, M, LT, N0],
    u     : Lub[H1, LT, L],
    tvs2  : IsRegularIterable[M[LT]] { type A = LT }, // tvs2, tev, and tcbf are required for the call to map below
    tev   : AdditiveCollection[M[LT]],
    f     : Factory[L, M[L]],
    tcbf  : BuildFrom[M[L], L, M[L]],
    tcbf2 : BuildFrom[M[LT], L, M[L]],
    tvs   : IsRegularIterable[M[L]] { type A = L }, // tvs, tcbf2, and ev are required for the call to +: below
    ev    : AdditiveCollection[M[L]]
  ): ToSized.Aux[H1 :: H2 :: T, M, L, Succ[N0]] =
    new ToSized[H1 :: H2 :: T, M] {
      type Lub = L
      type N = Succ[N0]
      def apply(l : H1 :: H2 :: T) =
        u.left(l.head) +: tts(l.tail).map(u.right)
    }
}
