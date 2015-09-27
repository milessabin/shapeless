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
      def apply() = annotation
    }

  implicit def materialize[A, T]: Annotation[A, T] = macro AnnotationMacros.materializeAnnotation[A, T]
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
      def apply() = annotations
    }

  implicit def materialize[A, T, Out <: HList]: Aux[A, T, Out] = macro AnnotationMacros.materializeAnnotations[A, T, Out]
}

class AnnotationMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def someTpe = typeOf[Some[_]].typeConstructor
  def noneTpe = typeOf[None.type]

    // FIXME Most of the content of this method is cut-n-pasted from generic.scala
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

  def materializeAnnotation[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val annTpe = weakTypeOf[A]

    if (!isProduct(annTpe))
      abort(s"$annTpe is not a case class-like type")

    val construct0 = construct(annTpe)

    val tpe = weakTypeOf[T]

    val annTreeOpt = tpe.typeSymbol.annotations.collectFirst {
      case ann if ann.tree.tpe =:= annTpe => construct0(ann.tree.children.tail)
    }

    annTreeOpt match {
      case Some(annTree) =>
        q"_root_.shapeless.Annotation.mkAnnotation[$annTpe, $tpe]($annTree)"
      case None =>
        abort(s"No $annTpe annotation found on $tpe")
    }
  }

  def materializeAnnotations[A: WeakTypeTag, T: WeakTypeTag, Out: WeakTypeTag]: Tree = {
    val annTpe = weakTypeOf[A]

    if (!isProduct(annTpe))
      abort(s"$annTpe is not a case class-like type")

    val construct0 = construct(annTpe)

    val tpe = weakTypeOf[T]

    val annTreeOpts =
      if (isProduct(tpe)) {
        val constructorSyms = tpe
          .member(termNames.CONSTRUCTOR)
          .asMethod
          .paramLists
          .flatten
          .map { sym => sym.name.decodedName.toString -> sym }
          .toMap

        fieldsOf(tpe).map { case (name, _) =>
          val paramConstrSym = constructorSyms(name.decodedName.toString)

          paramConstrSym.annotations.collectFirst {
            case ann if ann.tree.tpe =:= annTpe => construct0(ann.tree.children.tail)
          }
        }
      } else if (isCoproduct(tpe))
        ctorsOf(tpe).map { cTpe =>
          cTpe.typeSymbol.annotations.collectFirst {
            case ann if ann.tree.tpe =:= annTpe => construct0(ann.tree.children.tail)
          }
        }
      else
        abort(s"$tpe is not case class like or the root of a sealed family of types")

    val wrapTpeTrees = annTreeOpts.map {
      case Some(annTree) => appliedType(someTpe, annTpe) -> q"_root_.scala.Some($annTree)"
      case None => noneTpe -> q"_root_.scala.None"
    }

    val outTpe = mkHListTpe(wrapTpeTrees.map { case (aTpe, _) => aTpe })
    val outTree = wrapTpeTrees.foldRight(q"_root_.shapeless.HNil": Tree) {
      case ((_, bound), acc) => pq"_root_.shapeless.::($bound, $acc)"
    }

    q"_root_.shapeless.Annotations.mkAnnotations[$annTpe, $tpe, $outTpe]($outTree)"
  }
}
