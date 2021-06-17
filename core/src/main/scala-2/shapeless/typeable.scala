/*
 * Copyright (c) 2011-18 Miles Sabin
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
import scala.reflect.macros.blackbox

trait LowPriorityTypeableScalaCompat {
  implicit def dfltTypeable[T]: Typeable[T] = macro TypeableMacros.dfltTypeableImpl[T]
}

class TypeableMacros(val c: blackbox.Context) extends SingletonTypeUtils {
  import c.universe._
  import definitions.NothingClass

  val typeableTpe: Type = typeOf[Typeable[_]].typeConstructor
  val genericTpe: Type = typeOf[Generic[_]].typeConstructor

  def dfltTypeableImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val dealiased = tpe.dealias

    dealiased match {
      case t: TypeRef if t.sym == NothingClass =>
        c.abort(c.enclosingPosition, "No Typeable for Nothing")

      case ExistentialType(_, _) =>
        val tArgs = dealiased.typeArgs
        val normalized = appliedType(dealiased.typeConstructor, tArgs)
        val normalizedTypeable = c.inferImplicitValue(appliedType(typeableTpe, List(normalized)))
        if (normalizedTypeable.isEmpty)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
        normalizedTypeable

      case RefinedType(parents, decls) =>
        if (decls.nonEmpty)
          c.abort(c.enclosingPosition, "No Typeable for a refinement with non-empty decls")
        val parentTypeables = parents.filterNot(_ =:= typeOf[AnyRef]).map { parent =>
          c.inferImplicitValue(appliedType(typeableTpe, List(parent)))
        }
        if (parentTypeables.exists(_.isEmpty))
          c.abort(c.enclosingPosition, "Missing Typeable for parent of a refinement")

        q"""_root_.shapeless.Typeable.intersectionTypeable(
          _root_.scala.Array[_root_.shapeless.Typeable[_]](..$parentTypeables)
        )"""

      case pTpe if pTpe.typeArgs.nonEmpty =>
        val pSym = {
          val sym = pTpe.typeSymbol
          if (!sym.isClass)
            c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")

          val pSym0 = sym.asClass
          pSym0.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

          pSym0
        }

        if(!pSym.isCaseClass)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
        else
          mkCaseClassTypeable(tpe)

      case SingleType(_, v) if !v.isParameter =>
        q"""_root_.shapeless.Typeable.referenceSingletonTypeable[$tpe](
           $v.asInstanceOf[$tpe], ${nameOf(v)}, serializable = ${v.isModule}
        )"""

      case ConstantType(c) =>
        q"""_root_.shapeless.Typeable.valueSingletonTypeable[$tpe]($c.asInstanceOf[$tpe], ${nameOf(c.tpe)})"""

      // Outer#Inner is unsound in general since Inner can capture type members of Outer.
      case TypeRef(TypeRef(_, outer, args), inner, _) if !outer.isFinal || args.nonEmpty =>
        if (inner.isClass && inner.asClass.isCaseClass) mkCaseClassTypeable(tpe)
        else c.abort(c.enclosingPosition, s"No default Typeable for type projection $tpe")

      case _ =>
        val tsym = tpe.typeSymbol
        if (tsym.isStatic || tsym.isFinal || (tsym.isClass && tsym.asClass.isTrait)) {
          // scala/bug#4440 Final inner classes and traits have no outer accessor.
          q"_root_.shapeless.Typeable.namedSimpleTypeable(_root_.scala.Predef.classOf[$tpe], ${nameOf(tsym)})"
        } else {
          q"_root_.shapeless.Typeable.partialFunctionTypeable({ case x: $tpe => x }, ${nameOf(tsym)})"
        }
    }
  }

  private def mkCaseClassTypeable(tpe: Type): Tree = {
    // an unsafe accessor is one that isn't a case class accessor but has an abstract type.
    def isUnsafeAccessor(sym: TermSymbol): Boolean = {

      if (sym.isCaseAccessor) {
        false
      } else {
        val symType = sym.typeSignature.typeSymbol
        val isAbstract =
          symType.isAbstract ||           // Under Scala 2.10, isAbstract is spuriously false (macro-compat issue?)
            (symType != NoSymbol && symType.owner == tpe.typeSymbol) // So check the owner as well

        if (isAbstract) {
          sym.isVal ||
            sym.isVar ||
            (sym.isParamAccessor && !(sym.accessed.isTerm && sym.accessed.asTerm.isCaseAccessor))
        } else false
      }
    }

    val nonCaseAccessor = tpe.decls.exists {
      case sym: TermSymbol if isUnsafeAccessor(sym) => true
      case _ => false
    }
    if (nonCaseAccessor) {
      // there is a symbol, which is not a case accessor but a val,
      // var or param, so we won't be able to type check it safely:
      c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
    }
    val fields = tpe.decls.sorted collect {
      case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => sym.typeSignatureIn(tpe)
    }
    val fieldTypeables = fields.map { field => c.inferImplicitValue(appliedType(typeableTpe, List(field))) }
    if(fieldTypeables.contains(EmptyTree))
      c.abort(c.enclosingPosition, "Missing Typeable for field of a case class")

    q""" _root_.shapeless.Typeable.namedCaseClassTypeable(
      _root_.scala.Predef.classOf[$tpe], _root_.scala.Array[_root_.shapeless.Typeable[_]](..$fieldTypeables), ${nameOf(tpe)}
    )"""
  }

  private def nameOf(sym: Symbol): String =
    sym.name.decodedName.toString

  private def nameOf(tpe: Type): String =
    nameOf(tpe.typeSymbol)
}
