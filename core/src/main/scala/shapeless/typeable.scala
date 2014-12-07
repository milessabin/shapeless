/*
 * Copyright (c) 2011-14 Miles Sabin
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

/**
 * Type class supporting type safe cast.
 *
 * @author Miles Sabin
 */
trait Typeable[T] {
  def cast(t: Any): Option[T]
}

trait LowPriorityTypeable {
  implicit def dfltTypeable[T]: Typeable[T] = macro TypeableMacros.dfltTypeableImpl[T]
}

/**
 * Provides instances of `Typeable`. Also provides an implicit conversion which enhances arbitrary values with a
 * `cast[T]` method.
 */
object Typeable extends TupleTypeableInstances with LowPriorityTypeable {
  import java.{ lang => jl }
  import scala.collection.{ GenMap, GenTraversable }
  import scala.reflect.ClassTag
  import syntax.typeable._

  def apply[T](implicit castT: Typeable[T]) = castT

  case class ValueTypeable[T, B](cB: Class[B]) extends Typeable[T] {
    def cast(t: Any): Option[T] = {
      if(t != null && (cB isAssignableFrom t.getClass)) Some(t.asInstanceOf[T]) else None
    }
  }

  /** Typeable instance for `Byte`. */
  implicit val byteTypeable: Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte])
  /** Typeable instance for `Short`. */
  implicit val shortTypeable: Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short])
  /** Typeable instance for `Char`. */
  implicit val charTypeable: Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character])
  /** Typeable instance for `Int`. */
  implicit val intTypeable: Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer])
  /** Typeable instance for `Long`. */
  implicit val longTypeable: Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long])
  /** Typeable instance for `Float`. */
  implicit val floatTypeable: Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float])
  /** Typeable instance for `Double`. */
  implicit val doubleTypeable: Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double])
  /** Typeable instance for `Boolean`. */
  implicit val booleanTypeable: Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean])
  /** Typeable instance for `Unit`. */
  implicit val unitTypeable: Typeable[Unit] = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit])

  def isValClass[T](clazz: Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]

  /** Typeable instance for `Any`. */
  implicit val anyTypeable: Typeable[Any] =
    new Typeable[Any] {
      def cast(t: Any): Option[Any] = Some(t)
    }

  /** Typeable instance for `AnyVal`. */
  implicit val anyValTypeable: Typeable[AnyVal] =
    new Typeable[AnyVal] {
      def cast(t: Any): Option[AnyVal] = {
        if(t != null && isValClass(t.getClass)) Some(t.asInstanceOf[AnyVal]) else None
      }
    }

  /** Typeable instance for `AnyRef`. */
  implicit val anyRefTypeable: Typeable[AnyRef] =
    new Typeable[AnyRef] {
      def cast(t: Any): Option[AnyRef] = {
        if(t == null || isValClass(t.getClass)) None else Some(t.asInstanceOf[AnyRef])
      }
    }

  /** Typeable instance for simple monomorphic types */
  def simpleTypeable[T](erased: Class[T]): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] = {
        if(t != null && erased.isAssignableFrom(t.getClass)) Some(t.asInstanceOf[T]) else None
      }
    }

  /** Typeable instance for singleton value types */
  def valueSingletonTypeable[T](value: T): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] =
        if(t == value) Some(value) else None
    }

  /** Typeable instance for singleton reference types */
  def referenceSingletonTypeable[T <: AnyRef](value: T): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] =
        if(t.asInstanceOf[AnyRef] eq value) Some(value) else None
    }

  /** Typeable instance for intersection types with typeable parents */
  def intersectionTypeable[T](parents: List[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] = {
        if(t != null && parents.forall(_.cast(t).isDefined)) Some(t.asInstanceOf[T]) else None
      }
    }

  /** Typeable instance for `Option`. */
  implicit def optionTypeable[T](implicit castT: Typeable[T]): Typeable[Option[T]] =
    new Typeable[Option[T]] {
      def cast(t: Any): Option[Option[T]] = {
        if(t == null) None
        else if(t.isInstanceOf[Option[_]]) {
          val o = t.asInstanceOf[Option[_]]
          if(o.isEmpty) Some(t.asInstanceOf[Option[T]])
          else for(e <- o; _ <- e.cast[T]) yield t.asInstanceOf[Option[T]]
        } else None
      }
    }

  /** Typeable instance for `Either`. */
  implicit def eitherTypeable[A, B]
    (implicit castA: Typeable[Left[A, B]], castB: Typeable[Right[A, B]]): Typeable[Either[A, B]] =
      new Typeable[Either[A, B]] {
        def cast(t: Any): Option[Either[A, B]] = {
          t.cast[Left[A, B]] orElse t.cast[Right[A, B]]
        }
      }

  /** Typeable instance for `Left`. */
  implicit def leftTypeable[A, B](implicit castA: Typeable[A]): Typeable[Left[A, B]] =
    new Typeable[Left[A, B]] {
      def cast(t: Any): Option[Left[A, B]] = {
        if(t == null) None
        else if(t.isInstanceOf[Left[_, _]]) {
          val l = t.asInstanceOf[Left[_, _]]
          for(a <- l.a.cast[A]) yield t.asInstanceOf[Left[A, B]]
        } else None
      }
    }

  /** Typeable instance for `Right`. */
  implicit def rightTypeable[A, B](implicit castB: Typeable[B]): Typeable[Right[A, B]] =
    new Typeable[Right[A, B]] {
      def cast(t: Any): Option[Right[A, B]] = {
        if(t == null) None
        else if(t.isInstanceOf[Right[_, _]]) {
          val r = t.asInstanceOf[Right[_, _]]
          for(b <- r.b.cast[B]) yield t.asInstanceOf[Right[A, B]]
        } else None
      }
    }

  /** Typeable instance for `GenTraversable`.
   *  Note that the contents be will tested for conformance to the element type. */
  implicit def genTraversableTypeable[CC[X] <: GenTraversable[X], T]
    (implicit mCC: ClassTag[CC[_]], castT: Typeable[T]): Typeable[CC[T]] =
    new Typeable[CC[T]] {
      def cast(t: Any): Option[CC[T]] =
        if(t == null) None
        else if(mCC.runtimeClass isAssignableFrom t.getClass) {
          val cc = t.asInstanceOf[CC[Any]]
          if(cc.forall(_.cast[T].isDefined)) Some(t.asInstanceOf[CC[T]])
          else None
        } else None
    }

  /** Typeable instance for `Map`. Note that the contents will be tested for conformance to the key/value types. */
  implicit def genMapTypeable[M[X, Y], K, V]
    (implicit ev: M[K, V] <:< GenMap[K, V], mM: ClassTag[M[_, _]], castKV: Typeable[(K, V)]): Typeable[M[K, V]] =
    new Typeable[M[K, V]] {
      def cast(t: Any): Option[M[K, V]] =
        if(t == null) None
        else if(mM.runtimeClass isAssignableFrom t.getClass) {
          val m = t.asInstanceOf[GenMap[Any, Any]]
          if(m.forall(_.cast[(K, V)].isDefined)) Some(t.asInstanceOf[M[K, V]])
          else None
        } else None
    }

  /** Typeable instance for polymorphic case classes with typeable elements */
  def caseClassTypeable[T](erased: Class[T], fields: List[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] =
        if(classOf[Product].isAssignableFrom(erased) && erased.isAssignableFrom(t.getClass)) {
          val c = t.asInstanceOf[Product with T]
          val f = c.productIterator.toList
          if((f zip fields).forall { case (f, castF) => castF.cast(f).isDefined }) Some(c)
          else None
        } else None
      }

  /** Typeable instance for `HNil`. */
  implicit val hnilTypeable: Typeable[HNil] =
    new Typeable[HNil] {
      def cast(t: Any): Option[HNil] = if(t != null && t.isInstanceOf[HNil]) Some(t.asInstanceOf[HNil]) else None
    }

  /** Typeable instance for `HList`s. Note that the contents will be tested for conformance to the element types. */
  implicit def hlistTypeable[H, T <: HList](implicit castH: Typeable[H], castT: Typeable[T]): Typeable[H :: T] =
    new Typeable[H :: T] {
      def cast(t: Any): Option[H :: T] = {
        if(t == null) None
        else if(t.isInstanceOf[::[_, _ <: HList]]) {
          val l = t.asInstanceOf[::[_, _ <: HList]]
          for(hd <- l.head.cast[H]; tl <- (l.tail: Any).cast[T]) yield t.asInstanceOf[H :: T]
        } else None
      }
    }

  /** Typeable instance for `CNil`. */
  implicit val cnilTypeable: Typeable[CNil] =
    new Typeable[CNil] {
      def cast(t: Any): Option[CNil] = None
    }

  /**
   * Typeable instance for `Coproduct`s.
   * Note that the contents will be tested for conformance to one of the element types.
   */
  implicit def coproductTypeable[H, T <: Coproduct]
    (implicit castH: Typeable[H], castT: Typeable[T]): Typeable[H :+: T] =
    new Typeable[H :+: T] {
      def cast(t: Any): Option[H :+: T] = {
        t.cast[Inl[H, T]] orElse t.cast[Inr[H, T]]
      }
    }

  /** Typeable instance for `Inl`. */
  implicit def inlTypeable[H, T <: Coproduct](implicit castH: Typeable[H]): Typeable[Inl[H, T]] =
    new Typeable[Inl[H, T]] {
      def cast(t: Any): Option[Inl[H, T]] = {
        if(t == null) None
        else if(t.isInstanceOf[Inl[_, _ <: Coproduct]]) {
          val l = t.asInstanceOf[Inl[_, _ <: Coproduct]]
          for(hd <- l.head.cast[H]) yield t.asInstanceOf[Inl[H, T]]
        } else None
      }
    }

  /** Typeable instance for `Inr`. */
  implicit def inrTypeable[H, T <: Coproduct](implicit castT: Typeable[T]): Typeable[Inr[H, T]] =
    new Typeable[Inr[H, T]] {
      def cast(t: Any): Option[Inr[H, T]] = {
        if(t == null) None
        else if(t.isInstanceOf[Inr[_, _ <: Coproduct]]) {
          val r = t.asInstanceOf[Inr[_, _ <: Coproduct]]
          for(tl <- r.tail.cast[T]) yield t.asInstanceOf[Inr[H, T]]
        } else None
      }
    }
}

/**
 * Extractor for use of `Typeable` in pattern matching.
 *
 * Thanks to Stacy Curl for the idea.
 *
 * @author Miles Sabin
 */
trait TypeCase[T] {
  def unapply(t: Any): Option[T]
}

object TypeCase {
  import syntax.typeable._
  def apply[T: Typeable]: TypeCase[T] = new TypeCase[T] {
    def unapply(t: Any): Option[T] = t.cast[T]
  }
}

class TypeableMacros(val c: blackbox.Context) extends SingletonTypeUtils {
  import c.universe._
  import internal._

  def dfltTypeableImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]

    val typeableTpe = typeOf[Typeable[_]].typeConstructor
    val genericTpe = typeOf[Generic[_]].typeConstructor
    val AC = definitions.AnyClass
    val NC = definitions.NothingClass

    val dealiased = tpe.dealias
    dealiased match {
      case TypeRef(_, NC, _) =>
        c.abort(c.enclosingPosition, "No Typeable for Nothing")

      case ExistentialType(quantified, underlying) =>
        val tArgs = dealiased.typeArgs
        val normalized = appliedType(dealiased.typeConstructor, tArgs)

        if(normalized =:= dealiased)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")

        val normalizedTypeable = c.inferImplicitValue(appliedType(typeableTpe, List(normalized)))
        if(normalizedTypeable == EmptyTree)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
        normalizedTypeable

      case SingletonSymbolType(c) =>
        val sym = mkSingletonSymbol(c)
        q"""_root_.shapeless.Typeable.referenceSingletonTypeable[$tpe]($sym)"""

      case RefinedType(parents, decls) =>
        if(decls.nonEmpty)
          c.abort(c.enclosingPosition, "No Typeable for a refinement with non-empty decls")
        val parentTypeables = parents.map { parent => c.inferImplicitValue(appliedType(typeableTpe, List(parent))) }
        if(parentTypeables.exists(_ == EmptyTree))
          c.abort(c.enclosingPosition, "Missing Typeable for parent of a refinement")

        q"""_root_.shapeless.Typeable.intersectionTypeable(List(..$parentTypeables))"""

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
        val fields = tpe.decls.toList collect {
          case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => sym.typeSignatureIn(tpe)
        }
        val fieldTypeables = fields.map { field => c.inferImplicitValue(appliedType(typeableTpe, List(field))) }
        if(fieldTypeables.exists(_ == EmptyTree))
          c.abort(c.enclosingPosition, "Missing Typeable for field of a case class")

        q"""_root_.shapeless.Typeable.caseClassTypeable(classOf[$tpe], List(..$fieldTypeables))"""

      case SingleType(_, v) if !v.isParameter =>
        q"""_root_.shapeless.Typeable.referenceSingletonTypeable[$tpe]($v.asInstanceOf[$tpe])"""

      case ConstantType(c) =>
        q"""_root_.shapeless.Typeable.valueSingletonTypeable[$tpe]($c.asInstanceOf[$tpe])"""

      case other =>
        q"""_root_.shapeless.Typeable.simpleTypeable(classOf[$tpe])"""
    }
  }
}
