# shapeless: generic programming for Scala

[![Build Status](https://api.travis-ci.org/milessabin/shapeless.png?branch=master)](https://travis-ci.org/milessabin/shapeless)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/shapeless)
[![Maven Central](https://img.shields.io/maven-central/v/com.chuusai/shapeless_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/com.chuusai/shapeless_2.13)

**shapeless** is a type class and dependent type based generic programming
library for Scala.

**This branch contains shapeless 3 for Scala 3**

shapeless 3 was developed as part of a collaboration with Martin Odersky's
group at EPFL LAMP to develop [language-level support][mirror] for generic
programming for Scala 3. shapeless 3 is included in the [Dotty Community
Build][communitybuild].

## Current status

Included so far is a full implementation of poly-kinded type class derivation
with the generality of shapeless, the compile time performance of Magnolia and
a significantly reduced binary footprint.

Support is provided for deriving type classes indexed by types of kinds `*`
(eg.  `Monoid`, `Eq`, `Show`), `* -> *` (eg. `Functor`, `Traverse`, `Monad`)
`(* -> *) -> *)`, (eg. `FunctorK`, aka `HFunctor` in Haskell) and `* -> * -> *`
(eg.  `Bifunctor`). Support for additional kinds can be added fairly
straightforwardly with a small amount of additional boilerplate for each kind.

The first two of these kinds equal the power of shapeless 2's `Generic` and
`Generic1` (see their use in the [Kittens][kittens] type class derivation
library for [Cats][cats]). The remainder go considerably beyond.

Using shapeless 3 the derivation of a monoid for a Scala ADT is as simple as,

```scala
// Type class definition, eg. from Cats
trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  inline def apply[A](implicit ma: Monoid[A]): Monoid[A] = ma

  // Standard instance for Boolean
  implicit val monoidBoolean: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  // Standard instance for Int
  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x+y
  }
  // Standard instance for String
  implicit val monoidString: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x+y
  }

  // Generic instance
  implicit def monoidGen[A](implicit inst: K0.ProductInstances[Monoid, A]): Monoid[A] =
    new Monoid[A] {
      def empty: A = inst.construct([t] => (ma: Monoid[t]) => ma.empty)
      def combine(x: A, y: A): A =
        inst.map2(x, y)([t] => (mt: Monoid[t], t0: t, t1: t) => mt.combine(t0, t1))
    }

  // Hook for Dotty derives clause
  inline def derived[A](implicit gen: K0.ProductGeneric[A]): Monoid[A] =
    monoidGen(K0.mkProductInstances[Monoid, A](gen))
}

// ADT definition
case class ISB(i: Int, s: String, b: Boolean) derives Monoid
val a = ISB(23, "foo", true)
val b = ISB(13, "bar", false)

Monoid[ISB].combine(a, b) // == ISB(36, "foobar", true)
```

A similar derivation for [`Functor`][functor] allows the following,

```scala
sealed trait Opt[+A] derives Functor
object Opt {
  case class Sm[+A](value: A) extends Opt[A]
  case object Nn extends Opt[Nothing]
}

Functor[Opt].map(Sm("foo")).map(_.length) // == Sm(3)
```

We can even derive [higher order functors][functork] in almost exactly the same
way,

```scala
// An Option like type, with a default
sealed trait OptionD[T] {
  def fold: T = this match {
    case Given(t) => t
    case Default(t) => t
  }
}
object OptionD {
  case class Given[T](value: T) extends OptionD[T]
  case class Default[T](value: T) extends OptionD[T]

  val fold: OptionD ~> Id = [t] => (ot: OptionD[t]) => ot.fold
}

// A data type parameterized with an effect
case class OrderF[F[_]](
  item: F[String],
  quantity: F[Int]
) derives FunctorK

val incompleteOrder = OrderF(Given("Epoisse"), Default(1))
val completeOrder = FunctorK[OrderF].mapK(incompleteOrder)(OptionD.fold)
// == OrderF[Id]("Epoisse", 1)
```

## Roadmap

A backport to Scala 2 and an adaptation layer to ease migration from shapeless
2.x to shapeless 3.x are in progress. Other components of shapeless 2 will be
migrated to shapeless 3 as it evolves.

## Finding out more about the project

shapeless is part of the [Typelevel][typelevel] family of projects. It is an
Open Source project under the Apache License v2, hosted on [GitHub][source].
Binary artefacts are published to the [Sonatype OSS Repository Hosting
service][sonatype] and synced to Maven Central.

Most discussion of shapeless and generic programming in Scala happens on the
shapeless [Gitter channel][gitter].

## Participation

The shapeless project supports the [Scala Code of Conduct][codeofconduct] and
wants all of its channels (Gitter, github, etc.) to be welcoming environments
for everyone.

[codeofconduct]: https://www.scala-lang.org/conduct/
[typelevel]: http://typelevel.org/
[source]: https://github.com/milessabin/shapeless
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~shapeless
[gitter]: https://gitter.im/milessabin/shapeless
[mirror]: https://github.com/lampepfl/dotty/pull/6531
[communitybuild]: https://github.com/lampepfl/dotty/pull/6645
[kittens]: https://github.com/typelevel/kittens
[cats]: https://github.com/typelevel/cats
[functor]: https://github.com/milessabin/shapeless/blob/shapeless-3/core/src/test/scala/shapeless/type-classes.scala#L95-L122
[functork]: https://github.com/milessabin/shapeless/blob/shapeless-3/core/src/test/scala/shapeless/type-classes.scala#L124-L150
