/*
 * Copyright (c) 2015-9 Alexandre Archambault
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

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

trait AnnotationScalaCompat {
  inline given[A, T]: Annotation[A, T] = ${AnnotationMacros.singleAnnotationForType[A, T]}
  inline given[A, T]: Annotation[Option[A], T] = ${AnnotationMacros.singleOptAnnotationForType[A, T]}
}

trait AnnotationsScalaCompat {
  inline transparent given[A, T]: Annotations[A, T] = ${AnnotationMacros.allAnnotations[A, T]}
}

trait TypeAnnotationsScalaCompat {
  inline transparent given[A, T]: TypeAnnotations[A, T] = ${AnnotationMacros.allTypeAnnotations[A, T]}
}

trait AllAnnotationsScalaCompat {
  inline transparent given[T]: AllAnnotations[T] = ${AnnotationMacros.allAnnotationsForType[T]}
}

trait AllTypeAnnotationsScalaCompat {
  inline transparent given[T]: AllTypeAnnotations[T] = ${AnnotationMacros.allTypeAnnotationsForType[T]}
}

object AnnotationMacros {

  private def errorIfInvalidAnnotation[A: Type](using quotes: Quotes): Unit = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[A]
    val flags = tpe.typeSymbol.flags
    if (flags.is(Flags.Abstract) || flags.is(Flags.Trait)) {
      report.errorAndAbort(s"Bad annotation type ${tpe.show} is abstract")
    }
  }

  private def annotationsFromType(using quotes: Quotes)(enclosing: quotes.reflect.TypeRepr, tpe: quotes.reflect.TypeRepr): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    tpe match {
      case AnnotatedType(rest, term) =>
        term :: annotationsFromType(enclosing, rest)
      //case ClassInfo(_, _, parents) => parents.flatMap(fromType(enclosing, _))
      case ref @ TypeRef(_, _) if ref.typeSymbol.isAliasType => annotationsFromType(enclosing, enclosing.memberType(ref.typeSymbol))
      case TypeBounds(low, hi) if low == hi => annotationsFromType(enclosing, low)
      case _ =>
        Nil
    }
  }

  private def findAnnotation[A: Type](using quotes: Quotes)(tpe: quotes.reflect.TypeRepr, symbol: quotes.reflect.Symbol, typeAnnotations: Boolean = false): Option[Expr[A]] = {
    import quotes.reflect.*
    val annotationType = TypeRepr.of[A]

    val annotations = if (typeAnnotations)
      annotationsFromType(tpe, tpe.memberType(symbol))
    else
      symbol.annotations

    annotations.find(_.tpe <:< annotationType).map(_.asExprOf[A])
  }

  private def singleAnnotationForTypeCommon[A: Type, T: Type](using quotes: Quotes): Option[Expr[A]] = {
    import quotes.reflect.*
    errorIfInvalidAnnotation[A]
    findAnnotation[A](TypeRepr.of[T], TypeRepr.of[T].typeSymbol)
  }

  def singleAnnotationForType[A: Type, T: Type](using quotes: Quotes): Expr[Annotation[A, T]] = {
    import quotes.reflect.*
    singleAnnotationForTypeCommon[A, T] match {
      case Some(expr) => '{Annotation.mkAnnotation($expr)}
      case None => report.errorAndAbort(s"No ${Type.show[A]} annotation found on ${Type.show[T]}")
    }
  }

  def singleOptAnnotationForType[A: Type, T: Type](using quotes: Quotes): Expr[Annotation[Option[A], T]] = {
    import quotes.reflect.*
    singleAnnotationForTypeCommon[A, T] match {
      case Some(expr) => '{Annotation.mkAnnotation(Some($expr))}
      case None => '{Annotation.mkAnnotation(None)}
    }
  }

  private def productOrSumSymbols[T: Type](using quotes: Quotes): Seq[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]

    tpe.classSymbol match {
      case Some(clazzSym) if clazzSym.flags.is(Flags.Case) =>
        clazzSym.primaryConstructor.paramSymss.find(_.headOption.fold(false)(_.isTerm)).getOrElse(Nil)

      case Some(maybeSum) =>
        Expr.summon[Mirror.Of[T]] match {
          case Some('{$m: mt { type MirroredElemTypes = met}}) =>

            def extractTupleTypes(tpe: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tpe match {
              case AppliedType(_, List(x: TypeRepr, xs: TypeRepr)) => extractTupleTypes(xs, x :: acc)
              case _ => acc.reverse
            }

            extractTupleTypes(TypeRepr.of[met], Nil).map(_.typeSymbol)

          case _ =>
            report.errorAndAbort(s"No Mirror found for ${tpe.show}")
        }

      case None =>
        report.errorAndAbort(s"${tpe.show} is not case class or the root of a sealed family of types")
    }
  }

  def allAnnotationsCommon[A: Type, T: Type, Out](typeAnnotations: Boolean)(
    makeResult: [Acc <: HList] => Type[Acc] ?=> Expr[Acc] => Expr[Out]
  )(using quotes: Quotes): Expr[Out] = {
    import quotes.reflect.*
    errorIfInvalidAnnotation[A]
    val tpe = TypeRepr.of[T]
    val annotations = productOrSumSymbols[T].map { symbol =>
      findAnnotation[A](tpe, symbol, typeAnnotations) match {
        case Some(expr) =>'{Some($expr)}
        case None       => '{None}
      }
    }

    listExprToResult[HNil, Out]('{HNil: HNil}, annotations.reverse)(makeResult)
  }

  def allAnnotations[A: Type, T: Type](using quotes: Quotes): Expr[Annotations[A, T]] = {
    allAnnotationsCommon[A, T, Annotations[A, T]](false) { [Acc <: HList] => (tpe: Type[Acc]) ?=> (acc: Expr[Acc]) =>
      '{
      new Annotations[A, T] {
        override type Out = Acc
        override def apply(): Acc = $acc
      }
      }
    }
  }

  def allTypeAnnotations[A: Type, T: Type](using quotes: Quotes): Expr[TypeAnnotations[A, T]] = {
    allAnnotationsCommon[A, T, TypeAnnotations[A, T]](true) { [Acc <: HList] => (tpe: Type[Acc]) ?=> (acc: Expr[Acc]) =>
      '{
      new TypeAnnotations[A, T] {
        override type Out = Acc
        override def apply(): Acc = $acc
      }
      }
    }
  }

  def allAnnotationsForTypeCommon[T: Type, Out](typeAnnotations: Boolean)(
    makeResult: [Acc <: HList] => Type[Acc] ?=> Expr[Acc] => Expr[Out]
  )(using quotes: Quotes): Expr[Out] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val annotationLists = productOrSumSymbols[T].map { symbol =>
      val annotations = if(typeAnnotations)
        annotationsFromType(tpe, tpe.memberType(symbol))
      else
        symbol.annotations
      listExprToHList(annotations.map(_.asExpr))
    }
    listExprToResult[HNil, Out]('{HNil: HNil}, annotationLists.reverse)(makeResult)
  }

  def allAnnotationsForType[T: Type](using quotes: Quotes): Expr[AllAnnotations[T]] = {
    allAnnotationsForTypeCommon[T, AllAnnotations[T]](false) { [Acc <: HList] => (tpe: Type[Acc]) ?=> (acc: Expr[Acc]) =>
      '{
      new AllAnnotations[T] {
        override type Out = Acc
        override def apply(): Out = $acc
      }
      }
    }
  }

  def allTypeAnnotationsForType[T: Type](using quotes: Quotes): Expr[AllTypeAnnotations[T]] = {
    allAnnotationsForTypeCommon[T, AllTypeAnnotations[T]](true) { [Acc <: HList] => (tpe: Type[Acc]) ?=> (acc: Expr[Acc]) =>
      '{
      new AllTypeAnnotations[T] {
        override type Out = Acc
        override def apply(): Out = $acc
      }
      }
    }
  }
}
