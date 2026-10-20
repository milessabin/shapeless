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

trait LowPriorityTypeable {
  transparent inline implicit def dfltTypeable[T]: Typeable[T] = ${ TypeableMacros.dfltTypeableImpl[T] }
}

object TypeableMacros {
  def dfltTypeableImpl[T: Type](using Quotes): Expr[Typeable[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val dealiased = MacroUtils.normalize(tpe)

    def nameOf(sym: Symbol): String = sym.name.stripSuffix("$")

    def classOf(t: TypeRepr): Expr[Class[T]] =
      Ref(defn.Predef_classOf).appliedToType(t).asExprOf[Class[T]]

    def summonTypeable(t: TypeRepr): Option[Expr[Typeable[?]]] =
      Implicits.search(TypeRepr.of[Typeable].appliedTo(t)) match {
        case s: ImplicitSearchSuccess => Some(s.tree.asExprOf[Typeable[?]])
        case _: ImplicitSearchFailure => None
      }

    def summonAll(ts: List[TypeRepr]): Option[Expr[Array[Typeable[?]]]] = {
      val found = ts.map(summonTypeable)
      if (found.exists(_.isEmpty)) None else Some('{ Array[Typeable[?]](${ Varargs(found.flatten) }*) })
    }

    def isWildcard(t: TypeRepr): Boolean = t match {
      case _: TypeBounds => true
      case _ => false
    }

    def mkCaseClassTypeable(target: TypeRepr): Expr[Typeable[T]] = {
      val sym = target.typeSymbol
      val caseFields = sym.caseFields.filterNot(_.name.endsWith(" "))
      // an unsafe accessor is one that isn't a case class accessor but has an abstract type (as declared in the class)
      def isUnsafeAccessor(f: Symbol): Boolean =
        !caseFields.exists(_.name == f.name.trim) && {
          val ft = sym.typeRef.memberType(f).widenByName.typeSymbol
          ft.isTypeParam || ft.isAbstractType || ft.flags.is(Flags.Abstract) || ft.flags.is(Flags.Trait) || ft.owner == sym
        }
      if (sym.declaredFields.exists(isUnsafeAccessor))
        report.errorAndAbort(s"No default Typeable for parametrized type ${tpe.show}")
      summonAll(caseFields.map(f => target.memberType(f).widenByName)) match {
        case None => report.errorAndAbort("Missing Typeable for field of a case class")
        case Some(fields) => '{ Typeable.namedCaseClassTypeable(${ classOf(target) }, $fields, ${ Expr(nameOf(sym)) }) }
      }
    }

    def mkSimpleTypeable(target: TypeRepr): Expr[Typeable[T]] = {
      val sym = target.typeSymbol
      val name = Expr(nameOf(sym))
      val isStatic = sym.owner.isPackageDef || sym.owner.flags.is(Flags.Module) && sym.owner.isClassDef
      if (isStatic || sym.flags.is(Flags.Final) || sym.flags.is(Flags.Trait))
        '{ Typeable.namedSimpleTypeable(${ classOf(target) }, $name) }
      else
        '{ Typeable.partialFunctionTypeable({ case x: T @unchecked => x }, $name) }
    }

    dealiased match {
      case t if t =:= TypeRepr.of[Nothing] =>
        report.errorAndAbort("No Typeable for Nothing")

      case _: AndType =>
        def conjuncts(t: TypeRepr): List[TypeRepr] = t match {
          case AndType(l, r) => conjuncts(l) ++ conjuncts(r)
          case t => List(t)
        }
        summonAll(conjuncts(dealiased).filterNot(_ =:= TypeRepr.of[AnyRef])) match {
          case Some(parents) => '{ Typeable.intersectionTypeable[T]($parents) }
          case None => report.errorAndAbort("Missing Typeable for parent of a refinement")
        }

      case Refinement(_, _, _) =>
        report.errorAndAbort("No Typeable for a refinement with non-empty decls")

      case AppliedType(tycon, args) if args.forall(isWildcard) && tycon.typeSymbol.isClassDef =>
        val normalized = tycon.appliedTo(args.map(_ => TypeRepr.of[Any]))
        summonTypeable(normalized) match {
          case Some(t) => t.asExprOf[Typeable[T]]
          case None => report.errorAndAbort(s"No default Typeable for parametrized type ${tpe.show}")
        }

      case AppliedType(tycon, _) =>
        val sym = tycon.typeSymbol
        if (!sym.isClassDef || !sym.flags.is(Flags.Case))
          report.errorAndAbort(s"No default Typeable for parametrized type ${tpe.show}")
        mkCaseClassTypeable(dealiased)

      case tr: TermRef if !tr.termSymbol.flags.is(Flags.Param) =>
        val sym = tr.termSymbol
        val value = Ref.term(tr).asExprOf[T & AnyRef]
        '{
          Typeable.referenceSingletonTypeable[T & AnyRef]($value, ${ Expr(sym.name) }, serializable = ${ Expr(sym.flags.is(Flags.Module)) })
            .asInstanceOf[Typeable[T]]
        }

      case ConstantType(c) =>
        val value = Literal(c).asExprOf[T]
        '{ Typeable.valueSingletonTypeable[T]($value, ${ Expr(dealiased.widen.typeSymbol.name) }) }

      // Outer#Inner is unsound in general since Inner can capture type members of Outer.
      case TypeRef(outer @ (_: TypeRef | _: AppliedType), _) if !outer.typeSymbol.flags.is(Flags.Final) || outer.typeArgs.nonEmpty =>
        if (dealiased.typeSymbol.flags.is(Flags.Case)) mkCaseClassTypeable(dealiased)
        else report.errorAndAbort(s"No default Typeable for type projection ${tpe.show}")

      case t if t.typeSymbol.isClassDef =>
        mkSimpleTypeable(dealiased)

      case _ =>
        report.errorAndAbort(s"No default Typeable for ${tpe.show}")
    }
  }
}
