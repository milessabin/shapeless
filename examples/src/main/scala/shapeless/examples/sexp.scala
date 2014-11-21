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

import shapeless._

/*
 * This example shows how to write a simple serialiser/deserialiser
 * library for arbitrary coproducts (i.e. sealed traits) and products
 * (i.e. case clases) using S-Expressions as the domain.
 *
 * This implementation is a proof of concept for using `TypeClass`
 * for this purpose and is currently limited by the implementation.
 * When Shapeless 2.1 comes out, breaking changes will enable all
 * the missing features. At which point, expect to see a fuller
 * implementation in `org.ensime.sexp`.
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

// Example ADT that we want to serialise/deserialise. Due to
// limitations with the 2.0 TypeClass, these have to be top-level
package sexp.examples {
  sealed trait Super
  case class Foo(i: Int) extends Super
  case class Bar(s: String) extends Super
  case class Baz(i: Int, s: String) extends Super
  case class Wibble(foo: Super) extends Super
}

/**
 * shapeless-examples/runMain shapeless.examples.SexpExamples
 */
object SexpExamples extends App {
  import sexp.examples._

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
    // order is important --- maybe shapeless 2.1 can fix that?
    SexpProp("i", SexpAtom("13")),
    SexpProp("s", SexpAtom("blah"))
  ))

  // SETUP
  import SexpConvert.auto._
  //import TypeClass.deriveConstructors
  val creator = SexpConvert[Super]

  // DESERIALISATION
  assert(creator.deser(SexpNil) == None) // expected miss
  assert(creator.deser(fooSexp) == Some(foo))
  assert(creator.deser(barSexp) == Some(bar))
  assert(creator.deser(bazSexp) == Some(baz))

  //// need shapeless 2.1 --- Foo is coming through as PRODUCT not COPRODUCT
  // val wibble = creator.deser(
  //   SexpCons(SexpAtom("Wibble"),
  //     SexpCons(SexpAtom("Foo"),
  //       SexpProp("i", SexpAtom("13")))))
  // assert(wibble == Some(Wibble(Foo(13))), wibble)

  // SERIALISATION
  assert(creator.ser(foo) == fooSexp)
  assert(creator.ser(bar) == barSexp)
  assert(creator.ser(baz) == bazSexp)
}

trait SexpConvert[T] {
  def deser(s: Sexp): Option[T]
  def ser(t: T): Sexp
}

object SexpConvert extends LabelledTypeClassCompanion[SexpConvert] {
  // the SexpConvert[{String, Int}] are really "user land" concepts
  // hopefully shapeless 2.1 will allow these to be moved out of the
  // type class companion.
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
  /////////////////////////////

  def apply[T](implicit c: SexpConvert[T]) = c

  implicit def tc: LabelledTypeClass[SexpConvert] = new LabelledTypeClass[SexpConvert] {
    def emptyProduct = new SexpConvert[HNil] {
      def deser(s: Sexp) = if (s == SexpNil) Some(HNil) else None
      def ser(n: HNil) = SexpNil
    }

    def product[F, T <: HList](name: String, FHead: SexpConvert[F], FTail: SexpConvert[T]) = new SexpConvert[F :: T] {
      def deser(s: Sexp): Option[F :: T] = s match {
        case SexpProp((field, car), cdr) if field == name =>
          for {
            front <- FHead.deser(car)
            back <- FTail.deser(cdr)
          } yield front :: back

        case _ =>
          println("PRODUCT MISS = " + s)
          None
      }

      def ser(ft: F :: T): Sexp = {
        val car = SexpProp(name, FHead.ser(ft.head))
        FTail.ser(ft.tail) match {
          case SexpNil => car
          case cdr => SexpCons(car, cdr)
        }
      }
    }

    def emptyCoproduct = new SexpConvert[CNil] {
      def deser(s: Sexp): Option[CNil] = None
      def ser(t: CNil) = SexpNil
    }

    def coproduct[L, R <: Coproduct](
      name: String, CL: => SexpConvert[L], CR: => SexpConvert[R]
    ) = new SexpConvert[L :+: R] {
      def deser(s: Sexp): Option[L :+: R] = s match {
        case SexpCons(SexpAtom(impl), cdr) if impl == name =>
          CL.deser(cdr).map(Inl(_))
        case SexpCons(SexpAtom(impl), cdr) =>
          CR.deser(s).map(Inr(_))
        case _ =>
          println("COPRODUCT MISS " + s)
          None
      }

      def ser(lr: L :+: R): Sexp = lr match {
        case Inl(l) => SexpCons(SexpAtom(name), CL.ser(l))
        case Inr(r) => CR.ser(r)
      }
    }

    def project[F, G](instance: => SexpConvert[G], to: F => G, from: G => F) = new SexpConvert[F] {
      def deser(s: Sexp): Option[F] = instance.deser(s).map(from)
      def ser(t: F): Sexp = instance.ser(to(t))
    }
  }
}
