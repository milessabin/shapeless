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

trait AnnotationScalaCompat {
  transparent inline implicit def materialize[A, T]: Annotation[A, T] =
    ${ AnnotationMacros.materializeAnnotationRequired[A, T] }

  transparent inline implicit def materializeOption[A, T]: Annotation[Option[A], T] =
    ${ AnnotationMacros.materializeAnnotationOptional[A, T] }
}

// See `Materialized` for why each type class with an output type has two macros

trait AnnotationsScalaCompat extends AnnotationsScalaCompat0 {
  transparent inline implicit def materialize[A, T, Out <: HList]: Annotations.Aux[A, T, Out] =
    ${ AnnotationMacros.materializeVariableAnnotations[A, T, Out] }

  transparent inline implicit def materializeAnnotations[A, T]: Materialized[Annotations[A, T]] =
    ${ AnnotationMacros.materializedAnnotations[A, T] }
}

trait AnnotationsScalaCompat0 {
  implicit def materializeAux[A, T, Out <: HList](
    implicit as: Materialized[Annotations[A, T]], ev: as.value.Out =:= Out
  ): Annotations.Aux[A, T, Out] =
    ev.substituteCo[[o] =>> Annotations[A, T] { type Out = o }](as.value)
}

trait TypeAnnotationsScalaCompat extends TypeAnnotationsScalaCompat0 {
  transparent inline implicit def materialize[A, T, Out <: HList]: TypeAnnotations.Aux[A, T, Out] =
    ${ AnnotationMacros.materializeTypeAnnotations[A, T, Out] }

  transparent inline implicit def materializeTypeAnnotations[A, T]: Materialized[TypeAnnotations[A, T]] =
    ${ AnnotationMacros.materializedTypeAnnotations[A, T] }
}

trait TypeAnnotationsScalaCompat0 {
  implicit def materializeAux[A, T, Out <: HList](
    implicit as: Materialized[TypeAnnotations[A, T]], ev: as.value.Out =:= Out
  ): TypeAnnotations.Aux[A, T, Out] =
    ev.substituteCo[[o] =>> TypeAnnotations[A, T] { type Out = o }](as.value)
}

trait AllAnnotationsScalaCompat extends AllAnnotationsScalaCompat0 {
  transparent inline implicit def materialize[T, Out <: HList]: AllAnnotations.Aux[T, Out] =
    ${ AnnotationMacros.materializeAllVariableAnnotations[T, Out] }

  transparent inline implicit def materializeAllAnnotations[T]: Materialized[AllAnnotations[T]] =
    ${ AnnotationMacros.materializedAllAnnotations[T] }
}

trait AllAnnotationsScalaCompat0 {
  implicit def materializeAux[T, Out <: HList](
    implicit as: Materialized[AllAnnotations[T]], ev: as.value.Out =:= Out
  ): AllAnnotations.Aux[T, Out] =
    ev.substituteCo[[o] =>> AllAnnotations[T] { type Out = o }](as.value)
}

trait AllTypeAnnotationsScalaCompat extends AllTypeAnnotationsScalaCompat0 {
  transparent inline implicit def materialize[T, Out <: HList]: AllTypeAnnotations.Aux[T, Out] =
    ${ AnnotationMacros.materializeAllTypeAnnotations[T, Out] }

  transparent inline implicit def materializeAllTypeAnnotations[T]: Materialized[AllTypeAnnotations[T]] =
    ${ AnnotationMacros.materializedAllTypeAnnotations[T] }
}

trait AllTypeAnnotationsScalaCompat0 {
  implicit def materializeAux[T, Out <: HList](
    implicit as: Materialized[AllTypeAnnotations[T]], ev: as.value.Out =:= Out
  ): AllTypeAnnotations.Aux[T, Out] =
    ev.substituteCo[[o] =>> AllTypeAnnotations[T] { type Out = o }](as.value)
}

object AnnotationMacros {
  def materializedAnnotations[A: Type, T: Type](using Quotes): Expr[Materialized[Annotations[A, T]]] =
    MacroUtils.materialized(materializeAnnotations[A, T](typeAnnotation = false).asExprOf[Annotations[A, T]])

  def materializedTypeAnnotations[A: Type, T: Type](using Quotes): Expr[Materialized[TypeAnnotations[A, T]]] =
    MacroUtils.materialized(materializeAnnotations[A, T](typeAnnotation = true).asExprOf[TypeAnnotations[A, T]])

  def materializedAllAnnotations[T: Type](using Quotes): Expr[Materialized[AllAnnotations[T]]] =
    MacroUtils.materialized(materializeAllAnnotations[T](typeAnnotation = false).asExprOf[AllAnnotations[T]])

  def materializedAllTypeAnnotations[T: Type](using Quotes): Expr[Materialized[AllTypeAnnotations[T]]] =
    MacroUtils.materialized(materializeAllAnnotations[T](typeAnnotation = true).asExprOf[AllTypeAnnotations[T]])

  def materializeAnnotation[A: Type, T: Type](using Quotes): Option[Expr[A]] = {
    import quotes.reflect.*
    val annTpe = TypeRepr.of[A]
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    if (!isProductAnnotationType(annTpe)) report.errorAndAbort(s"${annTpe.show} is not a case class-like type")
    annotatedSymbols(tpe).iterator.flatMap(_.annotations).collectFirst {
      case ann if ann.tpe =:= annTpe => ann.asExprOf[A]
    }
  }

  def materializeAnnotationRequired[A: Type, T: Type](using Quotes): Expr[Annotation[A, T]] = {
    import quotes.reflect.*
    materializeAnnotation[A, T] match {
      case Some(ann) => '{ Annotation.mkAnnotation[A, T]($ann) }
      case None => report.errorAndAbort(s"No ${TypeRepr.of[A].show} annotation found on ${TypeRepr.of[T].show}")
    }
  }

  def materializeAnnotationOptional[A: Type, T: Type](using Quotes): Expr[Annotation[Option[A], T]] =
    materializeAnnotation[A, T] match {
      case Some(ann) => '{ Annotation.mkAnnotation[Option[A], T](Some($ann)) }
      case None => '{ Annotation.mkAnnotation[Option[A], T](None) }
    }

  def materializeVariableAnnotations[A: Type, T: Type, Out <: HList: Type](using Quotes): Expr[Annotations.Aux[A, T, Out]] =
    materializeAnnotations[A, T](typeAnnotation = false).asInstanceOf[Expr[Annotations.Aux[A, T, Out]]]

  def materializeTypeAnnotations[A: Type, T: Type, Out <: HList: Type](using Quotes): Expr[TypeAnnotations.Aux[A, T, Out]] =
    materializeAnnotations[A, T](typeAnnotation = true).asInstanceOf[Expr[TypeAnnotations.Aux[A, T, Out]]]

  def materializeAllVariableAnnotations[T: Type, Out <: HList: Type](using Quotes): Expr[AllAnnotations.Aux[T, Out]] =
    materializeAllAnnotations[T](typeAnnotation = false).asInstanceOf[Expr[AllAnnotations.Aux[T, Out]]]

  def materializeAllTypeAnnotations[T: Type, Out <: HList: Type](using Quotes): Expr[AllTypeAnnotations.Aux[T, Out]] =
    materializeAllAnnotations[T](typeAnnotation = true).asInstanceOf[Expr[AllTypeAnnotations.Aux[T, Out]]]

  def materializeAnnotations[A: Type, T: Type](typeAnnotation: Boolean)(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val annTpe = TypeRepr.of[A]
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    if (!isProductAnnotationType(annTpe)) report.errorAndAbort(s"${annTpe.show} is not a case class-like type")
    val elems = annotationTerms(tpe, typeAnnotation).map(_.find(_.tpe <:< annTpe) match {
      case Some(ann) => ann.tpe.widen.asType match {
        case '[a] => '{ Some[a](${ ann.asExprOf[a] }) }.asTerm
      }
      case None => '{ None }.asTerm
    })
    val hlist = MacroUtils.mkHList(elems)
    val companion = if (typeAnnotation) TypeRepr.of[TypeAnnotations.type] else TypeRepr.of[Annotations.type]
    Select.unique(Ref(companion.termSymbol), "mkAnnotations")
      .appliedToTypes(List(annTpe, tpe, hlist.tpe.widen))
      .appliedTo(hlist)
      .asExpr
  }

  def materializeAllAnnotations[T: Type](typeAnnotation: Boolean)(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    val hlist = MacroUtils.mkHList(annotationTerms(tpe, typeAnnotation).map(MacroUtils.mkHList))
    val companion = if (typeAnnotation) TypeRepr.of[AllTypeAnnotations.type] else TypeRepr.of[AllAnnotations.type]
    Select.unique(Ref(companion.termSymbol), "mkAnnotations")
      .appliedToTypes(List(tpe, hlist.tpe.widen))
      .appliedTo(hlist)
      .asExpr
  }

  /** Whether `tpe` is a case class like type, with any wildcard type arguments replaced by their upper bounds. */
  def isProductAnnotationType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    val instantiated = tpe.dealias match {
      case AppliedType(tycon, args) => tycon.appliedTo(args.map {
        case TypeBounds(_, hi) => hi
        case arg => arg
      })
      case other => other
    }
    GenericMacros.productInfo(instantiated).isDefined
  }

  /** The symbols carrying the annotations of the type `tpe` itself. */
  def annotatedSymbols(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    val term = tpe.termSymbol
    List(term, sym, if (sym.flags.is(Flags.Module)) sym.companionModule else Symbol.noSymbol).filterNot(_.isNoSymbol).distinct
  }

  /**
   * The declared parent types of the class `sym`, as written (ie. including any annotations). These are only available
   * from the class definition tree, which is present for classes compiled in the current run or with `-Yretain-trees`.
   */
  def parentTypes(using Quotes)(sym: quotes.reflect.Symbol): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    scala.util.Try(sym.tree).toOption.toList.flatMap {
      case cd: ClassDef => cd.parents.collect { case t: TypeTree => t.tpe }
      case _ => Nil
    }
  }

  /** The case class like annotations for each field of a product, or each constructor of a coproduct. */
  def annotationTerms(using Quotes)(tpe: quotes.reflect.TypeRepr, typeAnnotation: Boolean): List[List[quotes.reflect.Term]] = {
    import quotes.reflect.*

    def isProductAnnotation(ann: Term): Boolean = GenericMacros.productInfo(ann.tpe).isDefined

    def fromType(t: TypeRepr): List[Term] = t match {
      case AnnotatedType(underlying, ann) => fromType(underlying) :+ ann
      case tr: TypeRef if tr.typeSymbol.isAliasType => fromType(tr.translucentSuperType)
      case _ => Nil
    }

    GenericMacros.productInfo(tpe) match {
      case Some((fields, _, _)) =>
        val ctor = tpe.typeSymbol.primaryConstructor
        val params = ctor.paramSymss.flatten.filter(_.isTerm).map(p => p.name -> p).toMap
        val paramTpes = GenericMacros.explicitParamLists(
          GenericMacros.instantiate(tpe.memberType(ctor), tpe.typeArgs)
        ).flatten.toMap
        fields.map { case (name, _) =>
          val anns =
            if (typeAnnotation) paramTpes.get(name).toList.flatMap(fromType)
            else params.get(name).toList.flatMap(_.annotations.reverse)
          anns.filter(isProductAnnotation)
        }
      case None if GenericMacros.isCoproduct(tpe) =>
        GenericMacros.ctorsOf(tpe).map { ctor =>
          val anns =
            if (typeAnnotation) parentTypes(ctor.typeSymbol).flatMap(fromType)
            else annotatedSymbols(ctor).flatMap(_.annotations.reverse)
          anns.filter(isProductAnnotation)
        }
      case None =>
        report.errorAndAbort(s"${tpe.show} is not case class like or the root of a sealed family of types")
    }
  }
}
