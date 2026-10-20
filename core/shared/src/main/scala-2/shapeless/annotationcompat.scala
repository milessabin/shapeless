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

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait AnnotationScalaCompat {
  implicit def materialize[A, T]: Annotation[A, T] =
    macro AnnotationMacros.materializeAnnotationRequired[A, T]

  implicit def materializeOption[A, T]: Annotation[Option[A], T] =
    macro AnnotationMacros.materializeAnnotationOptional[A, T]
}

trait AnnotationsScalaCompat {
  implicit def materialize[A, T, Out <: HList]: Annotations.Aux[A, T, Out] =
    macro AnnotationMacros.materializeVariableAnnotations[A, T, Out]
}

trait TypeAnnotationsScalaCompat {
  implicit def materialize[A, T, Out <: HList]: TypeAnnotations.Aux[A, T, Out] =
    macro AnnotationMacros.materializeTypeAnnotations[A, T, Out]
}

trait AllAnnotationsScalaCompat {
  implicit def materialize[T, Out <: HList]: AllAnnotations.Aux[T, Out] =
    macro AnnotationMacros.materializeAllVariableAnnotations[T, Out]
}

trait AllTypeAnnotationsScalaCompat {
  implicit def materialize[T, Out <: HList]: AllTypeAnnotations.Aux[T, Out] =
    macro AnnotationMacros.materializeAllTypeAnnotations[T, Out]
}

class AnnotationMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def optionTpe: Type = typeOf[Option[_]].typeConstructor
  def someTpe: Type = typeOf[Some[_]].typeConstructor
  def noneTpe: Type = typeOf[None.type]

  private def someVal = objectRef[Some.type]
  private def noneVal = objectRef[None.type]
  private def annotationVal = objectRef[shapeless.Annotation.type]

  /**
   * FIXME Most of the content of this method is cut-n-pasted from generic.scala
   *
   * @return The AST of the `tpe` constructor.
   */
  def construct(tpe: Type): List[Tree] => Tree = {
    val sym = tpe.typeSymbol
    def hasCompanionMember(name: String) = {
      val member = sym.companion.typeSignature.member(TermName(name))
      member != NoSymbol && !isNonGeneric(member)
    }

    val useCompanion = sym.asClass.isCaseClass || hasCompanionMember("apply")
    if (useCompanion) args => q"${companionRef(tpe)}(..$args)" else args => q"new $tpe(..$args)"
  }

  def materializeAnnotation[A: WeakTypeTag, T: WeakTypeTag]: Option[Tree] = {
    val annTpe = weakTypeOf[A]
    val tpe = weakTypeOf[T]
    if (!isProduct(annTpe)) abort(s"$annTpe is not a case class-like type")
    val constructor = construct(annTpe)
    tpe.typeSymbol.annotations.collectFirst {
      case ann if ann.tree.tpe =:= annTpe => constructor(ann.tree.children.tail)
    }
  }

  def materializeAnnotationRequired[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val annTpe = weakTypeOf[A]
    val tpe = weakTypeOf[T]
    materializeAnnotation[A, T] match {
      case Some(annTree) => q"$annotationVal.mkAnnotation[$annTpe, $tpe]($annTree)"
      case None => abort(s"No $annTpe annotation found on $tpe")
    }
  }

  def materializeAnnotationOptional[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val optAnnTpe = appliedType(optionTpe, weakTypeOf[A])
    val tpe = weakTypeOf[T]
    materializeAnnotation[A, T] match {
      case Some(annTree) => q"$annotationVal.mkAnnotation[$optAnnTpe, $tpe]($someVal($annTree))"
      case None => q"$annotationVal.mkAnnotation[$optAnnTpe, $tpe]($noneVal)"
    }
  }

  def materializeVariableAnnotations[A: WeakTypeTag, T: WeakTypeTag, Out: WeakTypeTag]: Tree =
    materializeAnnotations[A, T, Out](typeAnnotation = false)

  def materializeAllVariableAnnotations[T: WeakTypeTag, Out: WeakTypeTag]: Tree =
    materializeAllAnnotations[T, Out](typeAnnotation = false)
    
  def materializeTypeAnnotations[A: WeakTypeTag, T: WeakTypeTag, Out: WeakTypeTag]: Tree =
    materializeAnnotations[A, T, Out](typeAnnotation = true)

  def materializeAllTypeAnnotations[T: WeakTypeTag, Out: WeakTypeTag]: Tree =
    materializeAllAnnotations[T, Out](typeAnnotation = true)
    
  def materializeAnnotations[A: WeakTypeTag, T: WeakTypeTag, Out: WeakTypeTag](typeAnnotation: Boolean): Tree = {
    val annTpe = weakTypeOf[A]
    val tpe = weakTypeOf[T]
    if (!isProduct(annTpe)) abort(s"$annTpe is not a case class-like type")
    val (annTypes, annTrees) = getAnnotationTreeOptions(tpe, typeAnnotation).map(_.find(_._1 <:< annTpe) match {
      case Some((annTpe, annTree)) => appliedType(someTpe, annTpe) -> q"$someVal($annTree)"
      case None => noneTpe -> noneVal
    }).unzip
    val tc = if (typeAnnotation) objectRef[TypeAnnotations.type] else objectRef[Annotations.type]
    q"$tc.mkAnnotations[$annTpe, $tpe, ${mkHListTpe(annTypes)}](${mkHListValue(annTrees)})"
  }

  def materializeAllAnnotations[T: WeakTypeTag, Out: WeakTypeTag](typeAnnotation: Boolean): Tree = {
    val tpe = weakTypeOf[T]
    val (annTypes, annTrees) = getAnnotationTreeOptions(tpe, typeAnnotation).map { annotations =>
      val (types, trees) = annotations.unzip
      mkHListTpe(types) -> mkHListValue(trees)
    }.unzip
    val tc = if (typeAnnotation) objectRef[AllTypeAnnotations.type] else objectRef[AllAnnotations.type]
    q"$tc.mkAnnotations[$tpe, ${mkHListTpe(annTypes)}](${mkHListValue(annTrees)})"
  }

  def getAnnotationTreeOptions(tpe: Type, typeAnnotation: Boolean): List[List[(Type, Tree)]] = {
    if (isProduct(tpe)) {
      lazy val constructorSyms = tpe
        .typeSymbol.asClass
        .primaryConstructor.asMethod
        .paramLists.iterator.flatten
        .map(sym => nameAsString(sym.name) -> sym)
        .toMap

      fieldsOf(tpe).map {
        case (name, _) =>
          extract(typeAnnotation, constructorSyms(nameAsString(name))).collect {
            case ann if isProduct(ann.tree.tpe) =>
              val construct1 = construct(ann.tree.tpe)
              (ann.tree.tpe, construct1(ann.tree.children.tail))
          }
      }
    } else if (isCoproduct(tpe)) {
      ctorsOf(tpe).map { cTpe =>
        extract(typeAnnotation, cTpe.typeSymbol).collect {
          case ann if isProduct(ann.tree.tpe) =>
            val construct1 = construct(ann.tree.tpe)
            (ann.tree.tpe, construct1(ann.tree.children.tail))
        }
      }
    } else {
      abort(s"$tpe is not case class like or the root of a sealed family of types")
    }
  }

  def extract(tpe: Boolean, s: Symbol): List[c.universe.Annotation] = {
    def fromType(t: Type): List[c.universe.Annotation] = t match {
      case AnnotatedType(annotations, _) => annotations.reverse
      case ClassInfoType(parents, _, _) => parents.flatMap(fromType)
      case TypeRef(_, sym, _) if sym.asType.isAliasType => extract(tpe, sym)
      case _ => Nil
    }

    if (tpe) fromType(s.typeSignature)
    else s.annotations
  }

}
