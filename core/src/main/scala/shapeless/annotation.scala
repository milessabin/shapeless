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

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Evidence that type `T` has annotation `A`, and provides an instance of the annotation.
 *
 * If type `T` has an annotation of type `A`, then an implicit `Annotation[A, T]` can be found, and its `apply` method
 * provides an instance of the annotation.
 *
 * Example:
 * {{{
 *   case class First(i: Int)
 *
 *   @First(3) trait Something
 *
 *
 *   val somethingFirst = Annotation[First, Something].apply()
 *   assert(somethingFirst == First(3))
 * }}}
 *
 * @tparam A: annotation type
 * @tparam T: annotated type
 *
 * @author Alexandre Archambault
 */
trait Annotation[A, T] extends Serializable {
  def apply(): A
}

object Annotation {
  def apply[A,T](implicit annotation: Annotation[A, T]): Annotation[A, T] = annotation

  def mkAnnotation[A, T](annotation: => A): Annotation[A, T] =
    new Annotation[A, T] {
      def apply(): A = annotation
    }

  implicit def materialize[A, T]: Annotation[A, T] = macro AnnotationMacros.materializeAnnotationRequired[A, T]
  implicit def materializeOption[A, T]: Annotation[Option[A], T] = macro AnnotationMacros.materializeAnnotationOptional[A, T]
}

/**
 * Provides the annotations of type `A` of the fields or constructors of case class-like or sum type `T`.
 *
 * If type `T` is case class-like, this type class inspects its fields and provides their annotations of type `A`. If
 * type `T` is a sum type, its constructor types are looked for annotations.
 *
 * Type `Out` is an HList having the same number of elements as `T` (number of fields of `T` if `T` is case class-like,
 * or number of constructors of `T` if it is a sum type). It is made of `None.type` (no annotation on corresponding
 * field or constructor) and `Some[A]` (corresponding field or constructor is annotated).
 *
 * Method `apply` provides an HList of type `Out` made of `None` (corresponding field or constructor not annotated)
 * or `Some(annotation)` (corresponding field or constructor has annotation `annotation`).
 *
 * Note that annotation types must be case class-like for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *
 *   case class CC(i: Int, @First("a") s: String)
 *
 *   sealed trait Base
 *   @First("b") case class BaseI(i: Int) extends Base
 *   case class BaseS(s: String) extends Base
 *
 *
 *   val ccFirsts = Annotations[First, CC]
 *   val baseFirsts = Annotations[First, Base]
 *
 *   // ccFirsts.Out is  None.type :: Some[First] :: HNil
 *   // ccFirsts.apply() is
 *   //   None :: Some(First("a")) :: HNil
 *
 *   // baseFirsts.Out is  Some[First] :: None.type :: HNil
 *   // baseFirsts.apply() is
 *   //   Some(First("b")) :: None :: HNil
 * }}}
 *
 * @tparam A: annotation type
 * @tparam T: case class-like or sum type, whose fields or constructors are annotated
 *
 * @author Alexandre Archambault
 */
trait Annotations[A,T] extends DepFn0 with Serializable {
  type Out <: HList
}

object Annotations {
  def apply[A,T](implicit annotations: Annotations[A,T]): Aux[A, T, annotations.Out] = annotations

  type Aux[A, T, Out0 <: HList] = Annotations[A, T] { type Out = Out0 }

  def mkAnnotations[A, T, Out0 <: HList](annotations: => Out0): Aux[A, T, Out0] =
    new Annotations[A, T] {
      type Out = Out0
      def apply(): Out = annotations
    }

  implicit def materialize[A, T, Out <: HList]: Aux[A, T, Out] = macro AnnotationMacros.materializeVariableAnnotations[A, T, Out]
}

/**
 * Provides the type annotations of type `A` of the fields or constructors of case class-like or sum type `T`.
 *
 * If type `T` is case class-like, this type class inspects its fields and provides their type annotations of type `A`. If
 * type `T` is a sum type, its constructor types are looked for type annotations.
 *
 * Type `Out` is an HList having the same number of elements as `T` (number of fields of `T` if `T` is case class-like,
 * or number of constructors of `T` if it is a sum type). It is made of `None.type` (no annotation on corresponding
 * field or constructor) and `Some[A]` (corresponding field or constructor is annotated).
 *
 * Method `apply` provides an HList of type `Out` made of `None` (corresponding field or constructor not annotated)
 * or `Some(annotation)` (corresponding field or constructor has annotation `annotation`).
 *
 * Note that type annotations must be case class-like for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *
 *   case class CC(i: Int, s: String @First("a"))
 *
 *   val ccFirsts = TypeAnnotations[First, CC]
 *
 *   // ccFirsts.Out is  None.type :: Some[First] :: HNil
 *   // ccFirsts.apply() is
 *   //   None :: Some(First("a")) :: HNil
 *
 * }}}
 *
 * This implementation is based on [[shapeless.Annotations]] by Alexandre Archambault.
 *
 * @tparam A: type annotation type
 * @tparam T: case class-like or sum type, whose fields or constructors are annotated
 *
 * @author Patrick Grandjean
 */
trait TypeAnnotations[A,T] extends DepFn0 with Serializable {
  type Out <: HList
}

object TypeAnnotations {
  def apply[A,T](implicit annotations: TypeAnnotations[A,T]): Aux[A, T, annotations.Out] = annotations

  type Aux[A, T, Out0 <: HList] = TypeAnnotations[A, T] { type Out = Out0 }

  def mkAnnotations[A, T, Out0 <: HList](annotations: => Out0): Aux[A, T, Out0] =
    new TypeAnnotations[A, T] {
      type Out = Out0
      def apply(): Out = annotations
    }

  implicit def materialize[A, T, Out <: HList]: Aux[A, T, Out] = macro AnnotationMacros.materializeTypeAnnotations[A, T, Out]
}

/**
 * Provides all variable annotations for the fields or constructors of case class-like or sum type `T`.
 *
 * If type `T` is case class-like, this type class inspects its fields and provides their variable annotations. If
 * type `T` is a sum type, its constructor types are looked for variable annotations as well.
 *
 * Type `Out` is an HList having the same number of elements as `T` (number of fields of `T` if `T` is case
 * class-like, or number of constructors of `T` if it is a sum type). It is made of `HNil` (no annotations for corresponding
 * field or constructor) or `HLists` (list of annotations for corresponding field or constructor).
 *
 * Method `apply` provides an HList of type `Out` made of `HNil` (corresponding field or constructor not annotated)
 * or `HList` (corresponding field or constructor has annotations).
 *
 * Note that variable annotations must be case class-like for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *   case class Second(i: Int)
 *
 *   case class CC(i: Int, @First("a") @Second(0) s: String)
 *
 *   val ccFirsts = AllAnnotations[CC]
 *
 *   // ccFirsts.Out is  HNil :: (First :: Second :: HNil) :: HNil
 *   // ccFirsts.apply() is
 *   //   HNil :: (First("a") :: Second(0) :: HNil) :: HNil
 *
 * }}}
 *
 * This implementation is based on [[shapeless.Annotations]] by Alexandre Archambault.
 *
 * @tparam T: case class-like or sum type, whose fields or constructors are annotated
 *
 * @author Patrick Grandjean
 */
trait AllAnnotations[T] extends DepFn0 with Serializable {
  type Out <: HList
}

object AllAnnotations {
  def apply[T](implicit annotations: AllAnnotations[T]): Aux[T, annotations.Out] = annotations

  type Aux[T, Out0 <: HList] = AllAnnotations[T] { type Out = Out0 }

  def mkAnnotations[T, Out0 <: HList](annotations: => Out0): Aux[T, Out0] =
    new AllAnnotations[T] {
      type Out = Out0
      def apply(): Out = annotations
    }

  implicit def materialize[T, Out <: HList]: Aux[T, Out] = macro AnnotationMacros.materializeAllVariableAnnotations[T, Out]
}

/**
 * Provides all type annotations for the fields or constructors of case class-like or sum type `T`.
 *
 * If type `T` is case class-like, this type class inspects its fields and provides their type annotations. If
 * type `T` is a sum type, its constructor types are looked for type annotations as well.
 *
 * Type `Out` is an HList having the same number of elements as `T` (number of fields of `T` if `T` is case
 * class-like, or number of constructors of `T` if it is a sum type). It is made of `HNil` (no annotations for corresponding
 * field or constructor) or `HLists` (list of annotations for corresponding field or constructor).
 *
 * Method `apply` provides an HList of type `Out` made of `HNil` (corresponding field or constructor not annotated)
 * or `HList` (corresponding field or constructor has annotations).
 *
 * Note that type annotations must be case class-like for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *   case class Second(i: Int)
 *
 *   case class CC(i: Int, s: String @First("a") @Second(0))
 *
 *   val ccFirsts = AllTypeAnnotations[CC]
 *
 *   // ccFirsts.Out is  HNil :: (First :: Second :: HNil) :: HNil
 *   // ccFirsts.apply() is
 *   //   HNil :: (First("a") :: Second(0) :: HNil) :: HNil
 *
 * }}}
 *
 * This implementation is based on [[shapeless.Annotations]] by Alexandre Archambault.
 *
 * @tparam T: case class-like or sum type, whose fields or constructors are annotated
 *
 * @author Patrick Grandjean
 */
trait AllTypeAnnotations[T] extends DepFn0 with Serializable {
  type Out <: HList
}

object AllTypeAnnotations {
  def apply[T](implicit annotations: AllTypeAnnotations[T]): Aux[T, annotations.Out] = annotations

  type Aux[T, Out0 <: HList] = AllTypeAnnotations[T] { type Out = Out0 }

  def mkAnnotations[T, Out0 <: HList](annotations: => Out0): Aux[T, Out0] =
    new AllTypeAnnotations[T] {
      type Out = Out0
      def apply(): Out = annotations
    }

  implicit def materialize[T, Out <: HList]: Aux[T, Out] = macro AnnotationMacros.materializeAllTypeAnnotations[T, Out]
}

class AnnotationMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def optionTpe: Type = typeOf[Option[_]].typeConstructor
  def someTpe: Type = typeOf[Some[_]].typeConstructor
  def noneTpe: Type = typeOf[None.type]

  /**
   * FIXME Most of the content of this method is cut-n-pasted from generic.scala
   *
   * @return The AST of the `tpe` constructor.
   */
  def construct(tpe: Type): List[Tree] => Tree = {
    // FIXME Cut-n-pasted from generic.scala
    val sym = tpe.typeSymbol
    val isCaseClass = sym.asClass.isCaseClass
    def hasNonGenericCompanionMember(name: String): Boolean = {
      val mSym = sym.companion.typeSignature.member(TermName(name))
      mSym != NoSymbol && !isNonGeneric(mSym)
    }

    if(isCaseClass || hasNonGenericCompanionMember("apply"))
      args => q"${companionRef(tpe)}(..$args)"
    else
      args => q"new $tpe(..$args)"
  }

  def materializeAnnotation[A: WeakTypeTag, T: WeakTypeTag]: Option[Tree] = {
    val annTpe = weakTypeOf[A]

    if (!isProduct(annTpe))
      abort(s"$annTpe is not a case class-like type")

    val construct0 = construct(annTpe)

    val tpe = weakTypeOf[T]

    tpe.typeSymbol.annotations.collectFirst {
      case ann if ann.tree.tpe =:= annTpe => construct0(ann.tree.children.tail)
    }
  }

  def materializeAnnotationRequired[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val annTpe = weakTypeOf[A]
    val tpe = weakTypeOf[T]

    materializeAnnotation[A, T] match {
      case Some(annTree) =>
        q"_root_.shapeless.Annotation.mkAnnotation[$annTpe, $tpe]($annTree)"
      case None =>
        abort(s"No $annTpe annotation found on $tpe")
    }
  }

  def materializeAnnotationOptional[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val optAnnTpe = appliedType(optionTpe, weakTypeOf[A])
    val tpe = weakTypeOf[T]

    materializeAnnotation[A, T] match {
      case Some(annTree) =>
        q"_root_.shapeless.Annotation.mkAnnotation[$optAnnTpe, $tpe](_root_.scala.Some($annTree))"
      case None =>
        q"_root_.shapeless.Annotation.mkAnnotation[$optAnnTpe, $tpe](_root_.scala.None)"
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

    if (!isProduct(annTpe))
      abort(s"$annTpe is not a case class-like type")
      
    val tpe = weakTypeOf[T]

    val annTreeOpts = getAnnotationTreeOptions(tpe, typeAnnotation).map { list =>
      list.find(_._1 =:= annTpe).map(_._2)
    }
    
    val wrapTpeTrees = annTreeOpts.map {
      case Some(annTree) => appliedType(someTpe, annTpe) -> q"_root_.scala.Some($annTree)"
      case None => noneTpe -> q"_root_.scala.None"
    }

    val outTpe = mkHListTpe(wrapTpeTrees.map { case (aTpe, _) => aTpe })
    val outTree = wrapTpeTrees.foldRight(q"_root_.shapeless.HNil": Tree) {
      case ((_, bound), acc) => pq"_root_.shapeless.::($bound, $acc)"
    }

    if (typeAnnotation) q"_root_.shapeless.TypeAnnotations.mkAnnotations[$annTpe, $tpe, $outTpe]($outTree)"
    else q"_root_.shapeless.Annotations.mkAnnotations[$annTpe, $tpe, $outTpe]($outTree)"
  }

  def materializeAllAnnotations[T: WeakTypeTag, Out: WeakTypeTag](typeAnnotation: Boolean): Tree = {
    val tpe = weakTypeOf[T]
    val annTreeOpts = getAnnotationTreeOptions(tpe, typeAnnotation)
    
    val wrapTpeTrees = annTreeOpts.map {
      case Nil =>
        mkHListTpe(Nil) -> q"(_root_.shapeless.HNil)"
      case list =>
        mkHListTpe(list.map(_._1)) -> list.foldRight(q"_root_.shapeless.HNil": Tree) {
          case ((_, bound), acc) => pq"_root_.shapeless.::($bound, $acc)"
        }
    }

    val outTpe = mkHListTpe(wrapTpeTrees.map { case (aTpe, _) => aTpe })
    val outTree = wrapTpeTrees.foldRight(q"_root_.shapeless.HNil": Tree) {
      case ((_, bound), acc) =>
        pq"_root_.shapeless.::($bound, $acc)"
    }

    if (typeAnnotation) q"_root_.shapeless.AllTypeAnnotations.mkAnnotations[$tpe, $outTpe]($outTree)"
    else q"_root_.shapeless.AllAnnotations.mkAnnotations[$tpe, $outTpe]($outTree)"
  }

  def getAnnotationTreeOptions(tpe: Type, typeAnnotation: Boolean): List[List[(Type, Tree)]] = {
    if (isProduct(tpe)) {
      val constructorSyms = tpe
        .member(termNames.CONSTRUCTOR)
        .asMethod
        .paramLists
        .flatten
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
