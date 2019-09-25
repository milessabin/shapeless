/*
 * Copyright (c) 2011-19 Miles Sabin
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

import scala.compiletime._
import scala.quoted._

/**
 * Type class supporting type safe cast.
 *
 * @author Miles Sabin
 */
trait Typeable[T] extends Serializable {
  def cast(t: Any): Option[T] = if (castable(t)) Some(t.asInstanceOf[T]) else None
  def castable(t: Any): Boolean
  def describe: String
  override def toString = s"Typeable[$describe]"
}

object syntax {
  object typeable {
    implicit class Ops[T](t: T) {
      /**
       * Cast the receiver to a value of type `U` if possible. This operation
       * will be as precise wrt erasure as possible given the in-scope
       * `Typeable` instances available.
       */
      inline def cast[U](given tu: Typeable[U]): Option[U] = tu.cast(t)

      /**
       * Test whether the receiver can be cast to a value of type `U`. This
       * operation will be as precise wrt erasure as possible given the
       * in-scope `Typeable` instances available.
       */
      inline def castable[U](given tu: Typeable[U]): Boolean = tu.castable(t)

      /**
       * Cast the receiver to a value of subtype `U` of the receiver's static
       * type if possible. This operation will be as precise wrt erasure as
       * possible given the in-scope `Typeable` instances available.
       */
      inline def narrowTo[U](given ev: U <:< T, tu: Typeable[U]): Option[U] = t.cast[U]
    }
  }
}

/**
 * Provides instances of `Typeable`.
 */
object Typeable extends Typeable0 {
  import java.{ lang => jl }
  import scala.reflect.ClassTag
  import syntax.typeable.given

  inline def apply[T](given tt: Typeable[T]): Typeable[T] = tt

  case class ValueTypeable[T, B](cB: Class[B], describe: String) extends Typeable[T] {
    def castable(t: Any): Boolean = t != null && cB.isInstance(t)
  }

  /** Typeable instance for `Byte`. */
  given byteTypeable: Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte], "Byte")
  /** Typeable instance for `Short`. */
  given shortTypeable: Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short], "Short")
  /** Typeable instance for `Char`. */
  given charTypeable: Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character], "Char")
  /** Typeable instance for `Int`. */
  given intTypeable: Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer], "Int")
  /** Typeable instance for `Long`. */
  given longTypeable: Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long], "Long")
  /** Typeable instance for `Float`. */
  given floatTypeable: Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float], "Float")
  /** Typeable instance for `Double`. */
  given doubleTypeable: Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double], "Double")
  /** Typeable instance for `Boolean`. */
  given booleanTypeable: Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean], "Boolean")
  /** Typeable instance for `Unit`. */
  given unitTypeable: Typeable[Unit] = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit], "Unit")

  def isAnyValClass[T](clazz: Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]

  /** Typeable instance for `Any`. */
  given anyTypeable: Typeable[Any] {
    def castable(t: Any): Boolean = true
    def describe = "Any"
  }

  /** Typeable instance for `AnyVal`. */
  given anyValTypeable: Typeable[AnyVal] {
    def castable(t: Any): Boolean = t != null && isAnyValClass(t.getClass)
    def describe = "AnyVal"
  }

  /** Typeable instance for `AnyRef`. */
  given anyRefTypeable: Typeable[AnyRef] {
    def castable(t: Any): Boolean = t != null && !isAnyValClass(t.getClass)
    def describe = "AnyRef"
  }

  /** Typeable instance for `Option`. */
  given optionTypeable[T](given tt: Typeable[T]): Typeable[Option[T]] {
    def castable(t: Any): Boolean =
      t match {
        case Some(e) => e.castable[T]
        case _ => false
      }
    def describe = s"Option[${tt.describe}]"
  }

  /** Typeable instance for `Either`. */
  given eitherTypeable[A, B](given ta: Typeable[A], tb: Typeable[B]): Typeable[Either[A, B]] {
    def castable(t: Any): Boolean =
      t match {
        case Left(l) => l.castable[A]
        case Right(r) => r.castable[B]
        case _ => false
      }
    def describe = s"Either[${ta.describe}, ${tb.describe}]"
  }

  /** Typeable instance for `Left`. */
  given leftTypeable[A, B](given ta: Typeable[A]): Typeable[Left[A, B]] {
    def castable(t: Any): Boolean =
      t match {
        case Left(l) => l.castable[A]
        case _ => false
      }
    def describe = s"Left[${ta.describe}]"
  }

  /** Typeable instance for `Right`. */
  given rightTypeable[A, B](given tb: Typeable[B]): Typeable[Right[A, B]] {
    def castable(t: Any): Boolean =
      t match {
        case Right(r) => r.castable[B]
        case _ => false
      }
    def describe = s"Right[${tb.describe}]"
  }

  /** Typeable instance for `Iterable`.  Note that the contents be will tested
   *  for conformance to the element type.
   */
  given iterableTypeable[CC[t] <: Iterable[t], T](given CCTag: ClassTag[CC[Any]], tt: Typeable[T]): Typeable[CC[T]] {
    def castable(t: Any): Boolean =
      t match {
        case (cc: CC[_] @unchecked) if CCTag.runtimeClass.isAssignableFrom(t.getClass) =>
          cc.forall(_.castable[T])
        case _ => false
      }
    def describe = s"${CCTag.runtimeClass.getSimpleName}[${tt.describe}]"
  }

  /** Typeable instance for `Map`. Note that the contents will be tested for
   *  conformance to the key/value types.
   */
  given mapTypeable[M[k, v] <: Map[k, v], K, V](given MTag: ClassTag[M[Any, Any]], tk: Typeable[K], tv: Typeable[V]): Typeable[M[K, V]] {
    def castable(t: Any): Boolean =
      t match {
        case (m: Map[Any, Any] @unchecked) if MTag.runtimeClass.isAssignableFrom(t.getClass) =>
          m.forall { case (k, v) => k.castable[K] && v.castable[V] }
        case _ => false
      }
    def describe = s"${MTag.runtimeClass.getSimpleName}[${tk.describe}, ${tv.describe}]"
  }

  /** Typeable instance for `HNil`. */
  given hnilTypeable: Typeable[HNil] {
    def castable(t: Any): Boolean = t.isInstanceOf[HNil]
    def describe = "HNil"
  }

  /** Typeable instance for `HList`s. Note that the contents will be tested for
   *  conformance to the element types.
   */
  given hconsTypeable[H, T <: HList](given th: Typeable[H], tt: Typeable[T]): Typeable[H :*: T] {
    def castable(t: Any): Boolean =
      t match {
        case l: :*:[_, _] => l.head.castable[H] && l.tail.castable[T]
        case _ => false
      }
    def describe = s"${th.describe} :*: ${tt.describe}"
  }

  /** Typeable instance for `CNil`. */
  given cnilTypeable: Typeable[CNil] {
    def castable(t: Any): Boolean = t.isInstanceOf[CNil] // should always be false
    def describe = "CNil"
  }

  /**
   * Typeable instance for `Coproduct`s.  Note that the contents will be tested
   * for conformance to one of the element types.
   */
  given cconsTypeable[H, T <: Coproduct](given th: Typeable[H], tt: Typeable[T]): Typeable[H :+: T] {
    def castable(t: Any): Boolean =
      t match {
        case c: Inl[_, _] => c.head.castable[H]
        case c: Inr[_, _] => c.tail.castable[T]
        case _ => false
      }
    def describe = s"${th.describe} :+: ${tt.describe}"
  }

  /** Typeable instance for `Inl`. */
  given inlTypeable[H, T <: Coproduct](given th: Typeable[H]): Typeable[Inl[H, T]] {
    def castable(t: Any): Boolean =
      t match {
        case c: Inl[_, _] => c.head.castable[H]
        case _ => false
      }
    def describe = s"Inl[${th.describe}}]"
  }

  /** Typeable instance for `Inr`. */
  given inrTypeable[H, T <: Coproduct](given tt: Typeable[T]): Typeable[Inr[H, T]] {
    def castable(t: Any): Boolean =
      t match {
        case c: Inr[_, _] => c.tail.castable[T]
        case _ => false
      }
    def describe = s"Inr[${tt.describe}}]"
  }
  /** Typeable instance for simple monomorphic types */
  def namedSimpleTypeable[T](clazz: Class[T], name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t != null && clazz.isAssignableFrom(t.getClass)
      def describe = name
    }

  /** Typeable instance for singleton value types */
  def valueSingletonTypeable[T](value: T, name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t == value
      def describe = s"$name($value)"
    }

  /**
   * Typeable instance for singleton reference types
   *
   * @param value The singleton value
   *
   * @param name The name of the singleton
   *
   * @param serializable Whether the instance should be serializable. For
   * singleton types of object definitions and symbols, this should be true,
   * since they preserve their identity after serialization/deserialization.
   * For other cases, it should be false, since the deserialized instance
   * would lose its singleton property.
   */
  def referenceSingletonTypeable[T](value: T, name: String, serializable: Boolean): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]
      def describe = s"$name.type"

      @throws(classOf[java.io.IOException])
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        if (serializable) out.defaultWriteObject()
        else throw new java.io.NotSerializableException("referenceSingletonTypeable")
      }
    }

  /** Typeable instance for intersection types with typeable conjuncts */
  def intersectionTypeable[T](parents: Seq[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t != null && parents.forall(_.castable(t))
      def describe = parents.map(_.describe) mkString " & "
    }

  /** Typeable instance for union types with typeable disjuncts */
  def unionTypeable[T](parents: Seq[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t != null && parents.exists(_.castable(t))
      def describe = parents.map(_.describe) mkString " | "
    }

  /** Typeable instance for tuple types with typeable elements */
  def tupleTypeable[T](elems: Seq[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean =
        t match {
          case p: Product if p.productArity == elems.length =>
            elems.iterator.zip(p.productIterator).forall { case (e, p) => e.castable(p) }
          case _ => false
        }

      def describe: String = elems.map(_.describe).mkString("(", ", ", ")")
    }

  /** Typeable instance for polymorphic case classes with typeable elements. */
  def namedCaseClassTypeable[T](clazz: Class[T], fields: Seq[Typeable[_]], name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean =
        t match {
          case p: Product if clazz.isAssignableFrom(t.getClass) =>
            fields.iterator.zip(p.productIterator).forall { case (f, p) => f.castable(p) }
          case _ => false
        }
      def describe = name
    }
}

trait Typeable0 {
  inline def mkDefaultTypeable[T]: Typeable[T] = ${ TypeableMacros.impl[T] }

  inline given [T]: Typeable[T] = mkDefaultTypeable[T]
}

object TypeableMacros {
  import Typeable._

  def impl[T: Type](given qctx: QuoteContext): Expr[Typeable[T]] = {
    import qctx.tasty._
    import util._

    val TypeableType = typeOf[Typeable[_]] match {
      case Type.IsAppliedType(tp) => tp.tycon
    }

    val target = typeOf[T]

    def isAbstract(tp: Type): Boolean =
      tp.typeSymbol.isAbstractType ||
        (tp match {
          case Type.IsAppliedType(tp) =>
            isAbstract(tp.tycon) || tp.args.exists {
              case IsType(tp) => isAbstract(tp)
              case _ => false
            }
          case _ => false
        })

    def normalize(tp: TypeOrBounds): Type = tp match {
      case TypeBounds(lo, _) => lo
      case IsType(tp) => tp
    }

    def simpleName(tp: TypeOrBounds): String =
      normalize(tp).dealias match {
        case Type.IsAppliedType(tp) =>
          simpleName(tp.tycon) + tp.args.map(simpleName).mkString("[", ", ", "]")
        case Type.TypeRef(_, name) => name
        case tp => tp.show
      }

    def collectConjuncts(tp: Type): List[Type] = tp match {
      case Type.IsAndType(tp) =>
        collectConjuncts(tp.left) ++ collectConjuncts(tp.right)
      case tp => List(tp)
    }

    def collectDisjuncts(tp: Type): List[Type] = tp match {
      case Type.IsOrType(tp) =>
        collectDisjuncts(tp.left) ++ collectDisjuncts(tp.right)
      case tp => List(tp)
    }

    def summonAllTypeables(tps: List[Type]): Option[Expr[Seq[Typeable[_]]]] = {
      val ttps = tps.map(tp => Type.AppliedType(TypeableType, List(tp)))
      val instances = ttps.flatMap(ttp => searchImplicit(ttp) match {
        case IsImplicitSearchSuccess(iss) => List(iss.tree.seal.cast[Typeable[_]])
        case IsImplicitSearchFailure(_) => Nil
      })

      if (tps.length == instances.length) Some(Expr.ofSeq(instances))
      else None
    }

    def mkCaseClassTypeable = {
      val sym = target.classSymbol.get
      val caseFields = sym.caseFields
      def fieldTpe(f: Symbol) = f.tree match {
        case IsValDef(tree) => tree.tpt.tpe
      }
      if (!sym.fields.forall(f => caseFields.contains(f) || !isAbstract(fieldTpe(f)))) {
        qctx.error(s"No Typeable for case class ${target.show} with non-case fields")
        '{???}
      } else {
        val fieldTps = caseFields.map(f => target.memberType(f))
        summonAllTypeables(fieldTps) match {
          case None =>
            qctx.error(s"Missing Typeable for field of case class ${target.show}")
            '{???}
          case Some(ftps) =>
            val clazz = Ref(defn.Predef_classOf).appliedToType(target).seal.cast[Class[T]]
            val name = Expr(simpleName(target))

            '{ namedCaseClassTypeable($clazz, $ftps, $name) }
        }
      }
    }

    def mkNamedSimpleTypeable = {
      val name = Expr(simpleName(target))
      val clazz = Ref(defn.Predef_classOf).appliedToType(target).seal.cast[Class[T]]
      '{ namedSimpleTypeable($clazz, $name) }
    }

    target.dealias match {
      case Type.IsTermRef(tp) =>
        val ident = Ident(tp).seal.cast[T]
        val sym = tp.termSymbol
        val name = Expr(sym.name.toString)
        val serializable = Expr(sym.flags.is(Flags.Object))
        '{ referenceSingletonTypeable[T]($ident, $name, $serializable) }

      case Type.ConstantType(Constant(c)) =>
        val value = Literal(Constant(c)).seal.cast[T]
        val name = Expr(target.widen.typeSymbol.name.toString)
        '{ valueSingletonTypeable[T]($value, $name) }

      case Type.IsTypeRef(tp) =>
        val qual = tp.qualifier match {
          case Type.IsThisType(tp) => tp.tref
          case IsType(tp) => tp
          case _ => null.asInstanceOf[Type]
        }

        val sym = tp.typeSymbol

        def normalizeModuleClass(sym: Symbol): Symbol =
          if (sym.flags.is(Flags.ModuleClass)) sym.companionModule else sym

        val owner = normalizeModuleClass(sym.owner)

        qual match {
          case _ if sym.flags.is(Flags.Case) => mkCaseClassTypeable
          case null =>
            mkNamedSimpleTypeable
          case Type.IsTypeRef(tp) if normalizeModuleClass(tp.typeSymbol) == owner =>
            mkNamedSimpleTypeable
          case Type.IsTermRef(tp) if normalizeModuleClass(tp.termSymbol) == owner =>
            mkNamedSimpleTypeable
          case _ =>
            qctx.error(s"No Typeable for type ${target.show} with a dependent prefix")
            '{???}
        }

      case Type.IsAppliedType(tp) =>
        val tycon = tp.tycon
        val args = tp.args

        if (tp <:< typeOf[Tuple]) {
          summonAllTypeables(args.map(normalize)) match {
            case Some(etps) =>
              '{ tupleTypeable($etps) }
            case None =>
              qctx.error(s"No Typeable for tuple type ${target.show} with missing elements(s)")
              '{???}
          }
        } else if (tp.typeSymbol.flags.is(Flags.Case)) mkCaseClassTypeable
        else {
          qctx.error(s"No Typeable for parametrized type ${target.show}")
          '{???}
        }

      case Type.IsAndType(tp) =>
        val conjuncts = collectConjuncts(tp)
        summonAllTypeables(conjuncts) match {
          case Some(ctps) =>
            '{ intersectionTypeable($ctps) }
          case None =>
            qctx.error(s"No Typeable for & type ${target.show} with missing conjunct(s)")
            '{???}
        }

      case Type.IsOrType(tp) =>
        val disjuncts = collectDisjuncts(tp)
        summonAllTypeables(disjuncts) match {
          case Some(dtps) =>
            '{ unionTypeable($dtps) }
          case None =>
            qctx.error(s"No Typeable for | type ${target.show} with missing disjunct(s)")
            '{???}
        }

      case other =>
        qctx.error(s"No Typeable for type ${target.show}")
        '{???}
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
trait TypeCase[T] extends Serializable {
  def unapply(t: Any): Option[T]
}

object TypeCase {
  import syntax.typeable.given
  def apply[T](given tt: Typeable[T]): TypeCase[T] =
    new TypeCase[T] {
      def unapply(t: Any): Option[T] = t.cast[T]
      override def toString = s"TypeCase[${tt.describe}]"
    }
}
