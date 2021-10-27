/*
 * Copyright (c) 2013-16 Miles Sabin
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

import shapeless.syntax.SingletonOps

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait WidenScalaCompat {

  implicit def materialize[T, Out]: Widen.Aux[T, Out] = macro SingletonTypeMacros.materializeWiden[T, Out]
}

trait SingletonTypeUtils extends ReprTypes {
  import c.universe._
  import internal._
  import decorators._

  def singletonOpsTpe: Type = typeOf[syntax.SingletonOps]

  object SingletonType {
    def unapply(value: Tree): Option[Type] = (value, value.tpe) match {
      case (Literal(const), _) => Some(constantType(const))
      case (_, keyType @ SingleType(_, v)) if !v.isParameter && !isValueClass(v) => Some(keyType)
      case (q"${sops: Tree}.narrow", _) if sops.tpe <:< singletonOpsTpe =>
        Some(sops.tpe.member(TypeName("T")).typeSignature)
      case _ => None
    }
  }

  def narrowValue(value: Tree): (Type, Tree) = value match {
    case Literal(const) =>
      val tpe = constantType(const)
      (tpe, q"$value.asInstanceOf[$tpe]")
    case _ =>
      (value.tpe, value)
  }

  def parseLiteralType(typeStr: String): Option[Type] = for {
    parsed <- util.Try(c.parse(typeStr)).toOption
    checked <- Option(c.typecheck(parsed, silent = true))
    if checked.nonEmpty
    tpe <- SingletonType.unapply(checked)
  } yield tpe

  def parseStandardType(typeStr: String): Option[Type] = for {
    parsed <- util.Try(c.parse(s"null.asInstanceOf[$typeStr]")).toOption
    checked <- Option(c.typecheck(parsed, silent = true))
    if checked.nonEmpty
  } yield checked.tpe

  def parseType(typeStr: String): Option[Type] =
    parseStandardType(typeStr) orElse parseLiteralType(typeStr)

  def typeCarrier(tpe: Type): Literal =
    mkTypeCarrier(tq"{ type T = $tpe }")

  def fieldTypeCarrier(tpe: Type): Literal =
    mkTypeCarrier(tq"""{
      type T = $tpe
      type ->>[V] = Field[V]
      type Field[V] = _root_.shapeless.labelled.FieldType[$tpe, V]
    }""")

  def mkTypeCarrier(tree: Tree): Literal = {
    val carrier = c.typecheck(tree, mode = c.TYPEmode).tpe

    // We can't yield a useful value here, so return Unit instead which is at least guaranteed
    // to result in a runtime exception if the value is used in term position.
    Literal(Constant(())).setType(carrier)
  }

  def isValueClass(sym: Symbol): Boolean = {
    val tSym = sym.typeSignature.typeSymbol
    tSym.isClass && tSym.asClass.isDerivedValueClass
  }
}

class SingletonTypeMacros(val c: whitebox.Context) extends SingletonTypeUtils with NatMacroDefns {
  import c.universe._
  import internal._

  def mkOps(sTpe: Type, w: Tree): Tree =
    q"${reify(SingletonOps)}.instance[$sTpe]($w)"

  def mkAttributedQualifier(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    global.gen.mkAttributedQualifier(gTpe).asInstanceOf[Tree]
  }

  @annotation.tailrec
  final def unrefine(tpe: Type): Type = tpe.dealias match {
    case RefinedType(List(parent), scope) if scope.isEmpty => unrefine(parent)
    case other => other
  }

  def extractSingletonValue(tpe: Type): Tree = unrefine(tpe) match {
    case ConstantType(const) => Literal(const)
    case singleton: SingleType => mkAttributedQualifier(singleton)
    case ThisType(sym) => This(sym)
    case ref @ TypeRef(_, sym, _) if sym.isModuleClass => mkAttributedQualifier(ref)
    case _ => c.abort(c.enclosingPosition, s"Type argument $tpe is not a singleton type")
  }

  def extractResult(value: Tree)(mkResult: (Type, Tree) => Tree): Tree =
    (value.tpe, value) match {
      case (tpe @ ConstantType(const), _) =>
        mkResult(tpe, Literal(const))

      case (tpe: SingleType, tree) =>
        mkResult(tpe, tree)

      case (_, tree: This) =>
        mkResult(thisType(tree.symbol), tree)

      case (_, tree) if (tree.symbol ne null) && tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
        val sym = tree.symbol.asTerm
        val pre = if (sym.owner.isClass) thisType(sym.owner) else NoPrefix
        val symTpe = singleType(pre, sym)
        mkResult(symTpe, q"$sym.asInstanceOf[$symTpe]")

      case _ =>
        c.abort(c.enclosingPosition, s"Expression $value does not evaluate to a constant or a stable reference value")
    }

  def inferInstance(tci: Type): Tree = {
    val inferred = c.inferImplicitValue(tci)
    if (inferred == EmptyTree)
      c.abort(c.enclosingPosition, s"Unable to resolve implicit value of type $tci")
    inferred
  }

  def materializeWiden[T: WeakTypeTag, Out: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    val wideTpe = tpe.widen
    if (wideTpe =:= tpe) c.abort(c.enclosingPosition, s"Don't know how to widen $tpe")
    else q"${reify(Widen)}.instance[$tpe, $wideTpe](${reify(Predef)}.identity)"
  }
}
