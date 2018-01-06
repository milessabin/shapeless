/*
 * Copyright (c) 2014 Sam Halliday (@fommil)
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

package shapeless.examples

import shapeless._, labelled.{ field, FieldType }

/*
 * This example shows how to write a simple serialiser/deserialiser
 * library for arbitrary coproducts (i.e. sealed traits) and products
 * (i.e. case classes) using S-Expressions as the domain.
 *
 * This implementation is a proof of concept for using `TypeClass` for
 * this purpose. Expect to see a fuller implementation in
 * `org.ensime.sexp` and `org.ensime.json`.
 */

// Our example serialised form
// http://en.wikipedia.org/wiki/S-expression
package sexp {
  sealed trait Sexp
  case class SexpCons(car: Sexp, cdr: Sexp) extends Sexp
  case class SexpAtom(value: String) extends Sexp
  case object SexpNil extends Sexp

  // convenient constructor and matcher for (key . (value . nil)) two-element lists
  object SexpProp {
    def apply(name: String, value: Sexp): Sexp =
      SexpCons(SexpAtom(name), SexpCons(value, SexpNil))

    // matches the car (name, value) and the cdr
    def unapply(cons: Sexp): Option[((String, Sexp), Sexp)] = cons match {
      case SexpCons(SexpAtom(name), SexpCons(value, SexpNil)) =>
        Some((name, value), SexpNil)
      case SexpCons(SexpCons(SexpAtom(name), SexpCons(value, SexpNil)), cdr) =>
        Some((name, value), cdr)
      case _ => None
    }
  }
}
import sexp._

// Example ADT that we want to serialise/deserialise.
package sexp.examples {
  sealed trait Super
  case class Foo(i: Int) extends Super
  case class Bar(s: String) extends Super
  case class Baz(i: Int, s: String) extends Super
  case class Wibble(foo: Super) extends Super
}

/**
 * An example Abstract Syntax Tree / family.
 */
package sexp.ast {
  sealed trait Token {
    def text: String
  }

  sealed trait RawToken extends Token
  case class Split(text: String) extends RawToken
  case class And(text: String) extends RawToken
  case class Or(text: String) extends RawToken

  sealed trait ContextualMarker extends RawToken
  case class Like(text: String) extends ContextualMarker
  case class Prefer(text: String) extends ContextualMarker
  case class Negate(text: String) extends ContextualMarker

  sealed trait TokenTree extends Token
  sealed trait ContextualToken extends TokenTree
  sealed trait CompressedToken extends TokenTree
  case class Unparsed(text: String) extends TokenTree
  case class AndCondition(left: TokenTree, right: TokenTree, text: String) extends TokenTree
  case class OrCondition(left: TokenTree, right: TokenTree, text: String) extends TokenTree

  case class Ignored(text: String = "") extends TokenTree
  case class Unclear(text: String = "") extends TokenTree

  object SpecialToken extends TokenTree {
    // to test case object serialisation
    def text = ""
  }

  sealed trait Term extends TokenTree {
    def field: DatabaseField
  }

  case class DatabaseField(column: String)
  case class FieldTerm(text: String, field: DatabaseField, value: String) extends Term
  case class BoundedTerm(
    text: String,
    field: DatabaseField,
    low: Option[String] = None,
    high: Option[String] = None,
    inclusive: Boolean = false) extends Term
  case class LikeTerm(term: FieldTerm, like: Option[Like]) extends Term {
    val text = like.map(_.text).getOrElse("")
    val field = term.field
  }
  case class PreferToken(tree: TokenTree, before: Option[Prefer], after: Option[Prefer]) extends TokenTree {
    val text = before.getOrElse("") + tree.text + after.getOrElse("")
  }
  case class InTerm(field: DatabaseField, value: String, text: String = "") extends CompressedToken
  case class QualifierToken(text: String, field: DatabaseField) extends ContextualToken with Term
}

/** Example AST with performance problems */
package sexp.big {

  sealed trait Base
  case object Foo01 extends Base
  case object Foo02 extends Base
  case object Foo03 extends Base
  case object Foo04 extends Base
  case object Foo05 extends Base
  case object Foo06 extends Base
  case object Foo07 extends Base
  case object Foo08 extends Base
  case object Foo09 extends Base
  case object Foo10 extends Base
  case object Foo11 extends Base
  case object Foo12 extends Base
  case object Foo13 extends Base
  case object Foo14 extends Base
  case object Foo15 extends Base
  case object Foo16 extends Base
  case object Foo17 extends Base
  case object Foo18 extends Base
  case object Foo19 extends Base
  case object Foo20 extends Base
  case object Foo21 extends Base
  case object Foo22 extends Base
  case object Foo23 extends Base
  case object Foo24 extends Base
  case object Foo25 extends Base
  case object Foo26 extends Base
  case object Foo27 extends Base
  case object Foo28 extends Base
  case object Foo29 extends Base
  case object Foo30 extends Base
  case object Foo31 extends Base
  case object Foo32 extends Base
  case object Foo33 extends Base
  case object Foo34 extends Base
  case object Foo35 extends Base
  case object Foo36 extends Base
  case object Foo37 extends Base
  case object Foo38 extends Base
  case object Foo39 extends Base
  case object Foo40 extends Base
  case object Foo41 extends Base
  case object Foo42 extends Base
  case object Foo43 extends Base
  case object Foo44 extends Base
  case object Foo45 extends Base
  case object Foo46 extends Base
  case object Foo47 extends Base
  case object Foo48 extends Base
  case object Foo49 extends Base
  case object Foo50 extends Base

}

/**
 * examples/runMain shapeless.examples.SexpExamples
 */
object SexpExamples extends App {
  import sexp.examples._
  import SexpUserConvert._

  // example instances and expected forms
  val foo = Foo(13)
  val fooSexp = SexpCons(
    SexpAtom("Foo"),
    SexpProp("i", SexpAtom("13"))
  )
  val bar = Bar("blah")
  val barSexp = SexpCons(
    SexpAtom("Bar"),
    SexpProp("s", SexpAtom("blah"))
  )
  val baz = Baz(13, "blah")
  val bazSexp = SexpCons(SexpAtom("Baz"), SexpCons(
    // order is important --- how can we address this?
    SexpProp("i", SexpAtom("13")),
    SexpProp("s", SexpAtom("blah"))
  ))
  val wibble = Wibble(Foo(13))
  val wibbleSexp =
    SexpCons(SexpAtom("Wibble"),
      SexpProp("foo",
        SexpCons(SexpAtom("Foo"),
          SexpProp("i", SexpAtom("13")))))

  // SETUP
  val creator = SexpConvert[Super]

  // DESERIALISATION
  assert(creator.deser(SexpNil) == None) // expected miss
  assert(creator.deser(fooSexp) == Some(foo))
  assert(creator.deser(barSexp) == Some(bar))
  assert(creator.deser(bazSexp) == Some(baz))
  assert(creator.deser(wibbleSexp) == Some(wibble))

  // SERIALISATION
  assert(creator.ser(foo) == fooSexp)
  assert(creator.ser(bar) == barSexp)
  assert(creator.ser(baz) == bazSexp)
  assert(creator.ser(wibble) == wibbleSexp)


  // a less trivial example
  import sexp.ast._
  val complex = SexpConvert[TokenTree]

  val token = QualifierToken("thing", DatabaseField("Source.THING"))
  val tokenSexp =
    SexpCons(SexpAtom("QualifierToken"),
      SexpCons(SexpProp("text", SexpAtom("thing")),
        SexpProp("field",
          SexpProp("column", SexpAtom("Source.THING")))))

  assert(complex.deser(tokenSexp) == Some(token))
  assert(complex.ser(token) == tokenSexp)


  // a performance bottleneck example
  import sexp.big._
  val slow = SexpConvert[Base]

}

trait SexpConvert[T] {
  def deser(s: Sexp): Option[T]
  def ser(t: T): Sexp
}

// define serialisation of "primitive" types
object SexpUserConvert {
  implicit def stringSexpConvert: SexpConvert[String] = new SexpConvert[String] {
    def deser(s: Sexp) = s match {
      case SexpAtom(s) => Some(s)
      case _ => None
    }
    def ser(s: String) = SexpAtom(s)
  }
  implicit def intSexpConvert: SexpConvert[Int] = new SexpConvert[Int] {
    def deser(s: Sexp) = s match {
      case SexpAtom(s) => util.Try(s.toInt).toOption
      case _ => None
    }
    def ser(i: Int) = SexpAtom(i.toString)
  }
  implicit def boolSexpConvert: SexpConvert[Boolean] = new SexpConvert[Boolean] {
    def deser(s: Sexp) = s match {
      case SexpNil => Some(false)
      case other => Some(true)
    }
    def ser(b: Boolean) = if (b) SexpAtom("t") else SexpNil
  }
  implicit def optSexpConvert[T](c: Lazy[SexpConvert[T]]): SexpConvert[Option[T]] = new SexpConvert[Option[T]] {
    def deser(s: Sexp) = s match {
      case SexpNil => None
      case other => Some(c.value.deser(other))
    }
    def ser(o: Option[T]) = o match {
      case None => SexpNil
      case Some(t) => c.value.ser(t)
    }
  }
}

object SexpConvert {
  def apply[T](implicit st: Lazy[SexpConvert[T]]): SexpConvert[T] = st.value

  implicit def deriveHNil: SexpConvert[HNil] =
    new SexpConvert[HNil] {
      def deser(s: Sexp) = if (s == SexpNil) Some(HNil) else None
      def ser(n: HNil) = SexpNil
    }

  implicit def deriveHCons[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      scv: Lazy[SexpConvert[V]],
      sct: Lazy[SexpConvert[T]]
    ): SexpConvert[FieldType[K, V] :: T] =
      new SexpConvert[FieldType[K, V] :: T] {
        def deser(s: Sexp): Option[FieldType[K, V] :: T] = s match {
          case SexpProp((label, car), cdr) if label == key.value.name =>
            for {
              front <- scv.value.deser(car)
              back <- sct.value.deser(cdr)
            } yield field[K](front) :: back

          case _ =>
            println("PRODUCT MISS = " + s)
            None
        }

        def ser(ft: FieldType[K, V] :: T): Sexp = {
          val car = SexpProp(key.value.name, scv.value.ser(ft.head))
          sct.value.ser(ft.tail) match {
            case SexpNil => car
            case cdr => SexpCons(car, cdr)
          }
        }
      }

  implicit def deriveCNil: SexpConvert[CNil] = new SexpConvert[CNil] {
    def deser(s: Sexp): Option[CNil] = None
    def ser(t: CNil) = SexpNil
  }

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
    (implicit
      key: Witness.Aux[K],
      scv: Lazy[SexpConvert[V]],
      sct: Lazy[SexpConvert[T]]
    ): SexpConvert[FieldType[K, V] :+: T] =
      new SexpConvert[FieldType[K, V] :+: T] {
        def deser(s: Sexp): Option[FieldType[K, V] :+: T] = s match {
          case SexpCons(SexpAtom(impl), cdr) if impl == key.value.name =>
            scv.value.deser(cdr).map(v => Inl(field[K](v)))
          case SexpCons(SexpAtom(impl), cdr) =>
            sct.value.deser(s).map(Inr(_))
          case _ =>
            println("COPRODUCT MISS " + s)
            None
        }

        def ser(lr: FieldType[K, V] :+: T): Sexp = lr match {
          case Inl(l) => SexpCons(SexpAtom(key.value.name), scv.value.ser(l))
          case Inr(r) => sct.value.ser(r)
        }
      }

  implicit def deriveInstance[F, G]
    (implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[SexpConvert[G]]): SexpConvert[F] =
      new SexpConvert[F] {
        def deser(s: Sexp): Option[F] = sg.value.deser(s).map(gen.from)
        def ser(t: F): Sexp = sg.value.ser(gen.to(t))
      }
}
