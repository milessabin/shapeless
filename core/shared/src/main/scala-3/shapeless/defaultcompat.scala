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

import scala.quoted.*

// See `Materialized` for why there are two macros

trait DefaultScalaCompat extends DefaultScalaCompat0 {
  transparent inline implicit def materialize[T, L <: HList]: Default.Aux[T, L] = ${ DefaultMacros.materialize[T, L] }

  transparent inline implicit def materializeDefault[T]: Materialized[Default[T]] = ${ DefaultMacros.materializedDefault[T] }
}

trait DefaultScalaCompat0 {
  implicit def materializeAux[T, L <: HList](
    implicit default: Materialized[Default[T]], ev: default.value.Out =:= L
  ): Default.Aux[T, L] =
    ev.substituteCo[[l] =>> Default[T] { type Out = l }](default.value)
}

object DefaultMacros {
  def materialize[T: Type, L <: HList: Type](using Quotes): Expr[Default.Aux[T, L]] =
    mkDefault[T].asInstanceOf[Expr[Default.Aux[T, L]]]

  def materializedDefault[T: Type](using Quotes): Expr[Materialized[Default[T]]] =
    MacroUtils.materialized(mkDefault[T])

  def mkDefault[T: Type](using Quotes): Expr[Default[T]] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    val sym = tpe.typeSymbol

    def build(defaults: List[Term]): Expr[Default[T]] = {
      val hlist = MacroUtils.mkHList(defaults)
      Select.unique(Ref(TypeRepr.of[Default.type].termSymbol), "mkDefaultByName")
        .appliedToTypes(List(tpe, hlist.tpe.widen))
        .appliedTo(hlist)
        .asExprOf[Default[T]]
    }

    def hasDefaultParams(method: Symbol): Boolean =
      method.paramSymss.flatten.exists(_.flags.is(Flags.HasDefault))

    if (tpe.isSingleton || sym.flags.is(Flags.Module)) build(Nil)
    else GenericMacros.productInfo(tpe) match {
      case None => report.errorAndAbort(s"${tpe.show} is not a case class or case class like")
      case Some((fields, _, _)) =>
        val ctor = sym.primaryConstructor
        // Within the companion itself refer to it via `this`: going via its accessor would re-enter its initialization
        val enclosing = List.unfold(Symbol.spliceOwner)(s => Option.when(!s.isNoSymbol)((s, s.owner)))
        val companionClass = sym.companionModule.moduleClass
        val companion =
          if (!companionClass.isNoSymbol && enclosing.contains(companionClass)) Some(This(companionClass))
          else GenericMacros.companionRef(tpe)
        val companionTpe = companion.map(_.tpe.widen)
        val applyOverloadsWithDefaults =
          companionTpe.toList.flatMap(_.typeSymbol.methodMember("apply")).count(hasDefaultParams)
        val hasUniqueDefaults = hasDefaultParams(ctor) && applyOverloadsWithDefaults <= 1

        val none = '{ None }.asTerm
        def some(value: Term, fieldTpe: TypeRepr): Term = fieldTpe.asType match {
          case '[f] => '{ Some[f](${ value.asExprOf[f] }) }.asTerm
        }

        def defaultFor(i: Int, fieldTpe: TypeRepr): Term = (for {
          comp <- companion
          compTpe <- companionTpe
          getter <- List(s"apply$$default$$${i + 1}", s"$$lessinit$$greater$$default$$${i + 1}")
            .flatMap(n => compTpe.typeSymbol.methodMember(n)).headOption
        } yield {
          val sel = Select(comp, getter)
          val value = compTpe.memberType(getter) match {
            case pt: PolyType => sel.appliedToTypes(tpe.typeArgs)
            case _ => sel
          }
          some(value, fieldTpe)
        }).getOrElse(none)

        build(
          if (hasUniqueDefaults) fields.zipWithIndex.map { case ((_, ft), i) => defaultFor(i, ft) }
          else fields.map(_ => none)
        )
    }
  }
}
