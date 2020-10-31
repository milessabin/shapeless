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

package shapeless3.typeable

import scala.annotation.tailrec
import scala.deriving._
import scala.quoted._

class ReflectionUtils[Q <: QuoteContext & Singleton](val q: Q) {
  implicit val qctx: Q = q
  import qctx.reflect._

  case class Mirror(
    MirroredType: TypeRepr,
    MirroredMonoType: TypeRepr,
    MirroredElemTypes: Seq[TypeRepr],
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
        val ConstantType(Constant.String(ml0)) = ml
        val mels0 = tupleTypeElements(mels).map { case ConstantType(Constant.String(l)) => l }
        Mirror(mt, mmt, mets0, ml0, mels0)
      }
    }

    def apply(tpe: TypeRepr): Option[Mirror] = {
      val MirrorType = TypeRepr.of[scala.deriving.Mirror]

      val mtpe = Refinement(MirrorType, "MirroredType", TypeBounds(tpe, tpe))
      val instance = Implicits.search(mtpe) match {
        case iss: ImplicitSearchSuccess => Some(iss.tree.seal.cast[scala.deriving.Mirror])
        case _: ImplicitSearchFailure => None
      }
      instance.flatMap(Mirror(_))
    }
  }

  def tupleTypeElements(tp: TypeRepr): List[TypeRepr] = {
    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp match {
      case AppliedType(pairTpe, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
      case _ => acc
    }
    loop(tp, Nil).reverse
  }

  def low(tp: TypeRepr): TypeRepr = tp match {
    case tp: TypeBounds => tp.low
    case tp => tp
  }

  def findMemberType(tp: TypeRepr, name: String): Option[TypeRepr] = tp match {
    case Refinement(_, `name`, tp) => Some(low(tp))
    case Refinement(parent, _, _) => findMemberType(parent, name)
    case AndType(left, right) => findMemberType(left, name).orElse(findMemberType(right, name))
    case _ => None
  }
}
