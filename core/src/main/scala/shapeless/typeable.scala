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

import VersionSpecifics._

/**
 * Type class supporting type safe cast.
 *
 * @author Miles Sabin
 */
trait Typeable[T] extends Serializable {
  def cast(t: Any): Option[T]
  def describe: String
  override def toString = s"Typeable[$describe]"
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
  import scala.reflect.ClassTag
  import syntax.typeable._

  def apply[T](implicit castT: Typeable[T]) = castT

  // This is supported by type arguments on patterns, available in Typelevel Scala 4+
  def unapply[T: Typeable](t: Any): Option[T] = t.cast[T]

  case class ValueTypeable[T, B](cB: Class[B], describe: String) extends Typeable[T] {
    def cast(t: Any): Option[T] = {
      if(t != null && cB.isInstance(t)) Some(t.asInstanceOf[T]) else None
    }
  }

  /** Typeable instance for `Byte`. */
  implicit val byteTypeable: Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte], "Byte")
  /** Typeable instance for `Short`. */
  implicit val shortTypeable: Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short], "Short")
  /** Typeable instance for `Char`. */
  implicit val charTypeable: Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character], "Char")
  /** Typeable instance for `Int`. */
  implicit val intTypeable: Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer], "Int")
  /** Typeable instance for `Long`. */
  implicit val longTypeable: Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long], "Long")
  /** Typeable instance for `Float`. */
  implicit val floatTypeable: Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float], "Float")
  /** Typeable instance for `Double`. */
  implicit val doubleTypeable: Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double], "Double")
  /** Typeable instance for `Boolean`. */
  implicit val booleanTypeable: Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean], "Boolean")
  /** Typeable instance for `Unit`. */
  implicit val unitTypeable: Typeable[Unit] = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit], "Unit")

  def isValClass[T](clazz: Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]

  /** Typeable instance for `Any`. */
  implicit val anyTypeable: Typeable[Any] =
    new Typeable[Any] {
      def cast(t: Any): Option[Any] = Some(t)
      def describe = "Any"
    }

  /** Typeable instance for `AnyVal`. */
  implicit val anyValTypeable: Typeable[AnyVal] =
    new Typeable[AnyVal] {
      def cast(t: Any): Option[AnyVal] = {
        if(t != null && isValClass(t.getClass)) Some(t.asInstanceOf[AnyVal]) else None
      }
      def describe = "AnyVal"
    }

  /** Typeable instance for `AnyRef`. */
  implicit val anyRefTypeable: Typeable[AnyRef] =
    new Typeable[AnyRef] {
      def cast(t: Any): Option[AnyRef] = {
        if(t == null || isValClass(t.getClass)) None else Some(t.asInstanceOf[AnyRef])
      }
      def describe = "AnyRef"
    }

  /** Typeable instance for simple monomorphic types */
  def simpleTypeable[T](erased: Class[T]): Typeable[T] = {
    namedSimpleTypeable(erased, safeSimpleName(erased))
  }

  /** Typeable instance for simple monomorphic types, specifying the name explicitly */
  def namedSimpleTypeable[T](erased: Class[T], name: => String): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] = {
        if(t != null && erased.isAssignableFrom(t.getClass)) Some(t.asInstanceOf[T]) else None
      }
      def describe = name
    }

  /** Typeable instance for singleton value types */
  def valueSingletonTypeable[T](value: T, name: String): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] =
        if(t == value) Some(value) else None
      def describe = s"$name($value)"
    }

  /** Typeable instance for singleton reference types (not serializable by default) */
  def referenceSingletonTypeable[T <: AnyRef](value: T, name: String): Typeable[T] =
    referenceSingletonTypeable(value, name, serializable = false)

  /**
   * Typeable instance for singleton reference types
   *
   * @param value The singleton value
   *
   * @param name The name of the singleton
   *
   * @param serializable Whether the instance should be
   * serializable. For singleton types of object definitions
   * and symbols, this should be true, since they preserve
   * their identity after serialization/deserialization.
   * For other cases, it should be false, since the deserialized
   * instance wouldn't work correctly.
   */
  def referenceSingletonTypeable[T <: AnyRef](value: T, name: String, serializable: Boolean): Typeable[T] =
    new Typeable[T] {

      def cast(t: Any): Option[T] =
        if(t.asInstanceOf[AnyRef] eq value) Some(value) else None

      def describe = s"$name.type"

      @throws(classOf[java.io.IOException])
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        if (serializable) out.defaultWriteObject()
        else throw new java.io.NotSerializableException("referenceSingletonTypeable")
      }
    }

  /** Typeable instance for intersection types with typeable parents */
  def intersectionTypeable[T](parents: Array[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] = {
        if(t != null && parents.forall(_.cast(t).isDefined)) Some(t.asInstanceOf[T]) else None
      }
      def describe = parents map(_.describe) mkString " with "
    }
  
  /** Typeable instance for `Option`. */
  implicit def optionTypeable[T](implicit castT: Typeable[T]): Typeable[Option[T]] =
    new Typeable[Option[T]]{
      def cast(t: Any): Option[Option[T]] = {
        if(t == null) None
        else if(t.isInstanceOf[Option[_]]) {
          val o = t.asInstanceOf[Option[_]]
          if(o.isEmpty) Some(t.asInstanceOf[Option[T]])
          else for(e <- o; _ <- e.cast[T]) yield t.asInstanceOf[Option[T]]
        } else None
      }
      def describe = s"Option[${castT.describe}]"
    }

  /** Typeable instance for `Either`. */
  implicit def eitherTypeable[A, B]
    (implicit castA: Typeable[A], castB: Typeable[B]): Typeable[Either[A, B]] =
      new Typeable[Either[A, B]] {
        def cast(t: Any): Option[Either[A, B]] = {
          t.cast[Left[A, B]] orElse t.cast[Right[A, B]]
        }
        def describe = s"Either[${castA.describe}, ${castB.describe}]"
      }

  /** Typeable instance for `Left`. */
  implicit def leftTypeable[A, B](implicit castA: Typeable[A]): Typeable[Left[A, B]] =
    new Typeable[Left[A, B]] {
      def cast(t: Any): Option[Left[A, B]] = {
        if(t == null) None
        else if(t.isInstanceOf[Left[_, _]]) {
          val l = t.asInstanceOf[Left[_, _]]
          for(a <- l.left.get.cast[A]) yield t.asInstanceOf[Left[A, B]]
        } else None
      }
      def describe = s"Left[${castA.describe}]"
    }

  /** Typeable instance for `Right`. */
  implicit def rightTypeable[A, B](implicit castB: Typeable[B]): Typeable[Right[A, B]] =
    new Typeable[Right[A, B]] {
      def cast(t: Any): Option[Right[A, B]] = {
        if(t == null) None
        else if(t.isInstanceOf[Right[_, _]]) {
          val r = t.asInstanceOf[Right[_, _]]
          for(b <- r.right.get.cast[B]) yield t.asInstanceOf[Right[A, B]]
        } else None
      }
      def describe = s"Right[${castB.describe}]"
    }

  /** Typeable instance for `Traversable`.
   *  Note that the contents be will tested for conformance to the element type. */
  implicit def genTraversableTypeable[CC[X] <: Iterable[X], T]
    (implicit mCC: ClassTag[CC[_]], castT: Typeable[T]): Typeable[CC[T] with Iterable[T]] =
    // Nb. the apparently redundant `with Iterable[T]` is a workaround for a
    // Scala 2.10.x bug which causes conflicts between this instance and `anyTypeable`.
    new Typeable[CC[T]] {
      def cast(t: Any): Option[CC[T]] =
        if(t == null) None
        else if(mCC.runtimeClass isAssignableFrom t.getClass) {
          val cc = t.asInstanceOf[CC[Any]]
          if(cc.forall(_.cast[T].isDefined)) Some(t.asInstanceOf[CC[T]])
          else None
        } else None
      def describe = s"${mCC.runtimeClass.getSimpleName}[${castT.describe}]"
    }

  /** Typeable instance for `Map`. Note that the contents will be tested for conformance to the key/value types. */
  implicit def genMapTypeable[M[X, Y], K, V]
    (implicit ev: M[K, V] <:< GenMap[K, V], mM: ClassTag[M[_, _]], castK: Typeable[K], castV: Typeable[V]): Typeable[M[K, V]] =
    new Typeable[M[K, V]] {
      def cast(t: Any): Option[M[K, V]] =
        if(t == null) None
        else if(mM.runtimeClass isAssignableFrom t.getClass) {
          val m = t.asInstanceOf[GenMap[Any, Any]]
          if(m.forall(_.cast[(K, V)].isDefined)) Some(t.asInstanceOf[M[K, V]])
          else None
        } else None
      def describe = s"${mM.runtimeClass.getSimpleName}[${castK.describe}, ${castV.describe}]"
    }

  /** Typeable instance for polymorphic case classes with typeable elements */
  def caseClassTypeable[T](erased: Class[T], fields: Array[Typeable[_]]): Typeable[T] =
    namedCaseClassTypeable(erased, fields, safeSimpleName(erased))

  /** Typeable instance for polymorphic case classes with typeable elements, specifying the name explicitly. */
  def namedCaseClassTypeable[T](erased: Class[T], fields: Array[Typeable[_]], name: => String): Typeable[T] =
    new Typeable[T] {
      def cast(t: Any): Option[T] =
        if(classOf[Product].isAssignableFrom(erased) && erased.isAssignableFrom(t.getClass)) {
          val c = t.asInstanceOf[Product with T]
          val f = c.productIterator.toList
          if((f zip fields).forall { case (f, castF) => castF.cast(f).isDefined }) Some(c)
          else None
        } else None
      def describe = {
        val typeParams = fields map(_.describe) mkString(",")
        s"$name[$typeParams]"
      }
    }

  /** Typeable instance for `HNil`. */
  implicit val hnilTypeable: Typeable[HNil] =
    new Typeable[HNil] {
      def cast(t: Any): Option[HNil] = if(t != null && t.isInstanceOf[HNil]) Some(t.asInstanceOf[HNil]) else None
      def describe = "HNil"
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
      def describe = s"${castH.describe} :: ${castT.describe}"
    }

  /** Typeable instance for `CNil`. */
  implicit val cnilTypeable: Typeable[CNil] =
    new Typeable[CNil] {
      def cast(t: Any): Option[CNil] = None
      def describe = "CNil"
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
      def describe = s"${castH.describe} :+: ${castT.describe}"
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
      def describe = s"Inl[${castH.describe}}]"
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
      def describe = s"Inr[${castT.describe}}]"
    }

  // Workaround for https://issues.scala-lang.org/browse/SI-5425
  private def safeSimpleName(erased: Class[_]): String =
    try {
      erased.getSimpleName
    } catch {
      case _: InternalError =>
        erased.getName
    }
}

/**
 * Extractor for use of `Typeable` in pattern matching.
 *
 * Thanks to Stacy Curl for the idea.
 *
 * @author Miles Sabin
 */
trait TypeCase[T] extends Serializable {
  def unapply(t: Any): Option[T]
}

object TypeCase {
  import syntax.typeable._
  def apply[T](implicit tt:Typeable[T]): TypeCase[T] = new TypeCase[T] {
    def unapply(t: Any): Option[T] = t.cast[T]
    override def toString = s"TypeCase[${tt.describe}]"
  }
}

@macrocompat.bundle
class TypeableMacros(val c: blackbox.Context) extends SingletonTypeUtils {
  import c.universe._
  import definitions.NothingClass

  val typeableTpe = typeOf[Typeable[_]].typeConstructor
  val genericTpe = typeOf[Generic[_]].typeConstructor

  def dfltTypeableImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]

    val dealiased = tpe.dealias

    dealiased match {
      case t: TypeRef if t.sym == NothingClass =>
        c.abort(c.enclosingPosition, "No Typeable for Nothing")

      case ExistentialType(_, underlying) =>
        val tArgs = dealiased.typeArgs
        val normalized = appliedType(dealiased.typeConstructor, tArgs)

        val normalizedTypeable = c.inferImplicitValue(appliedType(typeableTpe, List(normalized)))
        if(normalizedTypeable == EmptyTree)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
        normalizedTypeable

      case SingletonSymbolType(c) =>
        val sym = mkSingletonSymbol(c)
        val name = sym.symbol.name.toString
        q"""_root_.shapeless.Typeable.referenceSingletonTypeable[$tpe]($sym, $name, serializable = true)"""

      case RefinedType(parents, decls) =>
        if(decls.nonEmpty)
          c.abort(c.enclosingPosition, "No Typeable for a refinement with non-empty decls")
        val parentTypeables = parents.filterNot(_ =:= typeOf[AnyRef]).map { parent =>
          c.inferImplicitValue(appliedType(typeableTpe, List(parent)))
        }
        if(parentTypeables.exists(_ == EmptyTree))
          c.abort(c.enclosingPosition, "Missing Typeable for parent of a refinement")

        q"""
          _root_.shapeless.Typeable.intersectionTypeable(
            _root_.scala.Array[_root_.shapeless.Typeable[_]](..$parentTypeables)
          )
         """

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
        val name = v.name.toString
        q"""_root_.shapeless.Typeable.referenceSingletonTypeable[$tpe]($v.asInstanceOf[$tpe], $name, serializable = ${v.isModule})"""

      case ConstantType(c) =>
        val name = c.tpe.typeSymbol.name.toString
        q"""_root_.shapeless.Typeable.valueSingletonTypeable[$tpe]($c.asInstanceOf[$tpe], $name)"""

      case other =>
        /* There is potential unsoundness if we allow a simple cast between two
         * unparameterized types, if they contain values of an abstract type variable
         * from outside of their definition. Therefore, check to see if any values
         * have types that look different from the inside and outside of the type. */
        val closesOverType = other.decls.exists {
          case sym: TermSymbol if sym.isVal || sym.isVar || sym.isParamAccessor =>
            val rtpe = sym.typeSignature
            rtpe.asSeenFrom(tpe, tpe.typeSymbol) != rtpe.asSeenFrom(tpe, tpe.typeSymbol.owner)
          case _ => false
        }

        val tsym = tpe.typeSymbol
        if (closesOverType) {
          if (tsym.isClass && tsym.asClass.isCaseClass) {
            /* it appears to be sound to treat captured type variables as if they were
             * simply case class parameters, as they'll be checked by their own Typeables later. */
            mkCaseClassTypeable(tpe)
          } else {
            c.abort(c.enclosingPosition, s"No default Typeable for type $tpe capturing an outer type variable")
          }
        } else {
          q"""_root_.shapeless.Typeable.namedSimpleTypeable(classOf[$tpe], ${tsym.name.toString})"""
        }
    }
  }

  private def mkCaseClassTypeable(tpe: Type): Tree = {
    val nonCaseAccessor = tpe.decls.exists {
      case sym: TermSymbol if !sym.isCaseAccessor && (sym.isVal || sym.isVar ||
        (sym.isParamAccessor && !(sym.accessed.isTerm && sym.accessed.asTerm.isCaseAccessor))) => true
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

    val name = tpe.typeSymbol.name.toString

    q"""
        _root_.shapeless.Typeable.namedCaseClassTypeable(
          classOf[$tpe], _root_.scala.Array[_root_.shapeless.Typeable[_]](..$fieldTypeables), $name
        )
       """
  }
}
