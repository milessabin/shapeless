# shapeless: generic programming for Scala

[![Discord](https://img.shields.io/discord/632277896739946517.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/bSQBZA3Ced)
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

Included so far is a full implementation of type class derivation as flexible
as that of shapeless 2, but generalized across kinds. For derivation of regular
ADTs and type classes compile- and runtime performance are dramatically
improved over shapeless 2. This comes with a significantly reduced binary
footprint in client code.

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
import shapeless3.deriving.*

// Type class definition, eg. from Cats
trait Monoid[A]:
  def empty: A
  extension (x: A)
    @alpha("combine") def |+| (y: A): A

object Monoid:
  inline def apply[A](using ma: Monoid[A]): Monoid[A] = ma

  given Monoid[Unit] with
    def empty: Unit = ()
    def combine(x: Unit, y: Unit): Unit = ()

  given Monoid[Boolean] with
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y

  given Monoid[Int] with
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x+y

  given Monoid[String] with
    def empty: String = ""
    def combine(x: String, y: String): String = x+y

  given monoidGen[A](using inst: K0.ProductInstances[Monoid, A]): Monoid[A] with
    def empty: A =
      inst.construct([t] => (ma: Monoid[t]) => ma.empty)
    def combine(x: A, y: A): A =
      inst.map2(x, y)([t] => (mt: Monoid[t], t0: t, t1: t) => mt.combine(t0, t1))

  inline def derived[A](using gen: K0.ProductGeneric[A]): Monoid[A] = monoidGen

// ADT definition
case class ISB(i: Int, s: String, b: Boolean) derives Monoid
val a = ISB(23, "foo", true)
val b = ISB(13, "bar", false)

a |+| b // == ISB(36, "foobar", true)
```

A similar derivation for [`Functor`][functor] allows the following,

```scala
enum Opt[+A] derives Functor: 
  case Sm[+A](value: A)
  case Nn

Sm("foo").map(_.length) // == Sm(3)
```

We can even derive [higher order functors][functork] in almost exactly the same
way,

```scala
// An Option like type, with a default
enum OptionD[T]:
  case Given(value: T)
  case Default(value: T)

  def fold: T = this match {
    case Given(t) => t
    case Default(t) => t
  }

object OptionD:
  val fold: OptionD ~> Id = [t] => (ot: OptionD[t]) => ot.fold

// A data type parameterized with an effect
case class OrderF[F[_]](
  item: F[String],
  quantity: F[Int]
) derives FunctorK

val incompleteOrder = OrderF(Given("Epoisse"), Default(1))
val completeOrder = FunctorK[OrderF].mapK(incompleteOrder)(OptionD.fold)
// == OrderF[Id]("Epoisse", 1)
```
## Getting started

shapeless 3 is available for Scala 3.0.0. To include the deriving module in
your project add the following to your build,

```
libraryDependencies += "org.typelevel" %% "shapeless3-deriving" % "3.0.0"
```

## Finding out more about the project

shapeless is part of the [Typelevel][typelevel] family of projects. It is an
Open Source project under the Apache License v2, hosted on [GitHub][source].
Binary artefacts are published to the [Sonatype OSS Repository Hosting
service][sonatype] and synced to Maven Central.

Most discussion of shapeless and generic programming in Scala happens on the
shapeless channel of the [Typelevel Discord][discord].

## Participation

The shapeless project supports the [Scala Code of Conduct][codeofconduct] and
wants all of its channels (Gitter, github, etc.) to be welcoming environments
for everyone.

[codeofconduct]: https://www.scala-lang.org/conduct/
[typelevel]: http://typelevel.org/
[source]: https://github.com/milessabin/shapeless
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~shapeless
[gitter]: https://gitter.im/milessabin/shapeless
[discord]: https://discord.gg/bSQBZA3Ced
[mirror]: https://dotty.epfl.ch/docs/reference/contextual/derivation.html
[communitybuild]: https://github.com/lampepfl/dotty/tree/master/community-build/community-projects
[kittens]: https://github.com/typelevel/kittens
[cats]: https://github.com/typelevel/cats
[functor]: https://github.com/milessabin/shapeless/blob/shapeless-3/modules/deriving/src/test/scala/shapeless3/deriving/type-classes.scala#L135-L156 
[functork]:https://github.com/milessabin/shapeless/blob/shapeless-3/modules/deriving/src/test/scala/shapeless3/deriving/type-classes.scala#L329-L347 
