/*
 * Copyright (c) 2019 Miles Sabin
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
import scala.deriving._
import scala.quoted._

class ReflectionUtils[Q <: QuoteContext & Singleton](val q: Q) {
  implicit val qctx: Q = q
  import qctx.tasty._

  case class Mirror(
    MirroredType: Type,
    MirroredMonoType: Type,
    MirroredElemTypes: Seq[Type],
    MirroredLabel: String,
    MirroredElemLabels: Seq[String]
  )

  object Mirror {
    def apply(mirror: Expr[scala.deriving.Mirror]): Option[Mirror] = {
      val mirrorTpe = mirror.unseal.tpe.widen
      for {
        mt   <- findMemberType(mirrorTpe, "MirroredType")
        mmt  <- findMemberType(mirrorTpe, "MirroredMonoType")
        mets <- findMemberType(mirrorTpe, "MirroredElemTypes")
        ml   <- findMemberType(mirrorTpe, "MirroredLabel")
        mels <- findMemberType(mirrorTpe, "MirroredElemLabels")
      } yield {
        val mets0 = tupleTypeElements(mets)
        val Type.ConstantType(Constant(ml0: String)) = ml
        val mels0 = tupleTypeElements(mels).map { case Type.ConstantType(Constant(l: String)) => l }
        Mirror(mt, mmt, mets0, ml0, mels0)
      }
    }
  }

  def tupleTypeElements(tp: Type): List[Type] = {
    @tailrec def loop(tp: Type, acc: List[Type]): List[Type] = tp match {
      case Type.AppliedType(pairTpe, List(IsType(hd), IsType(tl))) => loop(tl, hd :: acc)
      case _ => acc
    }
    loop(tp, Nil).reverse
  }

  def low(tp: TypeOrBounds): Type = tp match {
    case IsType(tp) => tp
    case IsTypeBounds(tp) => tp.low
  }

  def findMemberType(tp: Type, name: String): Option[Type] = tp match {
    case Type.Refinement(_, `name`, tp) => Some(low(tp))
    case Type.Refinement(parent, _, _) => findMemberType(parent, name)
    case Type.AndType(left, right) => findMemberType(left, name).orElse(findMemberType(right, name))
    case _ => None
  }
}
