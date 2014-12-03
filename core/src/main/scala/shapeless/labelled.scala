/*
 * Copyright (c) 2014 Miles Sabin
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

import scala.reflect.macros.whitebox
import scala.util.Try

import syntax.SingletonOps

object labelled {
  /**
   * The type of fields with keys of singleton type `K` and value type `V`.
   */
  type FieldType[K, V] = V with KeyTag[K, V]
  trait KeyTag[K, V]

  /**
   * Yields a result encoding the supplied value with the singleton type `K' of its key.
   */
  def field[K] = new FieldBuilder[K]

  class FieldBuilder[K] {
    def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

/**
 * Polymorphic function that allows modifications on record fields while preserving the
 * original key types.
 *
 * @author Dario Rexin
 */
trait FieldPoly extends Poly1 {
  import labelled._

  class FieldCaseBuilder[A, T] {
    def apply[Res](fn: A => Res) = new Case[FieldType[T, A]] {
      type Result = FieldType[T, Res]
      val value: Function1[A :: HNil, FieldType[T, Res]] =
        (l: A :: HNil) => field[T](fn(l.head))
    }
  }

  def atField[A](w: Witness) = new FieldCaseBuilder[A, w.T]
}

/**
 * Field with values of type `V`.
 *
 * Record keys of this form should be objects which extend this trait. Keys may also be arbitrary singleton typed
 * values, however keys of this form enforce the type of their values.
 *
 * @author Miles Sabin
 */
trait FieldOf[V] {
  import labelled._

  def ->>(v: V): FieldType[this.type, V] = field[this.type](v)
}

class LabelledMacros(val c: whitebox.Context) {
  import labelled._
  import c.universe.{ Try => _, _ }
  import internal.constantType

  val hconsTpe = typeOf[::[_, _]].typeConstructor
  val hnilTpe = typeOf[HNil]
  val cconsTpe = typeOf[:+:[_, _]].typeConstructor
  val cnilTpe = typeOf[CNil]
  val fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor
  val singletonOpsTpe = typeOf[SingletonOps]
  val SymTpe = typeOf[scala.Symbol]
  val atatTpe = typeOf[tag.@@[_,_]].typeConstructor

  object LiteralSymbol {
    def unapply(t: Tree): Option[Constant] = t match {
      case q""" scala.Symbol.apply(${c.universe.Literal(c: Constant)}) """ => Some(c)
      case _ => None
    }
  }

  object SingletonKeyType {
    def mkSingletonSymbolType(c: Constant): Type =
      appliedType(atatTpe, List(SymTpe, constantType(c)))

    def unapply(t: Tree): Option[Type] = (t, t.tpe) match {
      case (c.universe.Literal(k: Constant), _) => Some(constantType(k))
      case (LiteralSymbol(k: Constant), _) => Some(mkSingletonSymbolType(k))
      case (_, keyType: SingleType) => Some(keyType)
      case (q""" $sops.narrow """, _) if sops.tpe <:< singletonOpsTpe =>
        Some(sops.tpe.member(TypeName("T")).typeSignature)
      case _ => None
    }
  }

  def recordTypeImpl(tpeSelector: c.Tree): c.Tree =
    labelledTypeImpl(tpeSelector, "record", hnilTpe, hconsTpe)

  def unionTypeImpl(tpeSelector: c.Tree): c.Tree =
    labelledTypeImpl(tpeSelector, "union", cnilTpe, cconsTpe)

  def labelledTypeImpl(tpeSelector: c.Tree, variety: String, nilTpe: Type, consTpe: Type): c.Tree = {
    import c.universe.{ Try => _, _ }
    import internal._, decorators._

    def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe))

    val q"${tpeString: String}" = tpeSelector
    val fields = tpeString.split(",").map(_.trim).map(_.split("->").map(_.trim)).map {
      case Array(key, valueTpe) =>
        val tpe =
          (for {
            parsed <- Try(c.parse(s"($key, null.asInstanceOf[$valueTpe])")).toOption
            checked = c.typecheck(parsed, silent = true)
            if checked.nonEmpty
          } yield
            checked match {
              case q""" (${SingletonKeyType(keyType)}, $v) """ => (keyType, v.tpe)
              case _ =>
                c.abort(c.enclosingPosition, s"$checked has the wrong shape for a $variety field")
            }
          ).getOrElse(c.abort(c.enclosingPosition, s"Malformed type $tpeString"))
        tpe
      case other =>
        c.abort(c.enclosingPosition, s"Malformed $variety type $tpeString")
    }

    val labelledTpe =
      fields.foldRight(nilTpe) { case ((keyTpe, valueTpe), acc) =>
        val fieldTpe = mkFieldTpe(keyTpe, valueTpe)
        appliedType(consTpe, List(fieldTpe, acc))
      }

    val carrier = c.typecheck(tq"{ type T = $labelledTpe }", mode = c.TYPEmode).tpe

    // We can't yield a useful value here, so return Unit instead which is at least guaranteed
    // to result in a runtime exception if the value is used in term position.
    c.universe.Literal(Constant(())).setType(carrier)
  }
}
