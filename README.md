# shapeless: generic programming for Scala

[![Build Status](https://api.travis-ci.org/milessabin/shapeless.png?branch=master)](https://travis-ci.org/milessabin/shapeless)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/shapeless)
[![Maven Central](https://img.shields.io/maven-central/v/com.chuusai/shapeless_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/com.chuusai/shapeless_2.13)

**shapeless** is a type class and dependent type based generic programming
library for Scala.

**This branch contains an early preview of shapeless 3 for Dotty/Scala 3**

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

Using shapeless 3 the derivation of a functor for a Scala ADT is as simple as,

```scala
// Type class definition
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  inline def apply[F[_]](implicit ff: Functor[F]): Functor[F] = ff

  implicit val functorId: Functor[Id] = new Functor[Id] {
    def map[A, B](a: A)(f: A => B): B = f(a)
  }

  implicit def functorConst[T]: Functor[Const[T]] = new Functor[Const[T]] {
    def map[A, B](t: T)(f: A => B): T = t
  }

  // Generic implementation
  implicit def functorGen[F[_]](implicit inst: => K1.Instances[Functor, F]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))
    }

  // Hook for Dotty derives clause
  inline def derived[F[_]](implicit gen: K1.Generic[F]): Functor[F] =
    functorGen(K1.mkInstances[Functor, F](gen))
}

// ADT definition with derives clause
sealed trait Opt[+T] derives Functor
object Opt {
  case class Sm[+T](t: T) extends Opt[T]
  case object Nn extends Opt[Nothing]
}

Functor[Opt].map(Sm("foo"))(_.length) // == Sm(3)
```

**Please Note** &mdash; currently there is an issue with separate compilation
in Dotty which means that the test at `core/src/test/shapeless/deriving.scala`
must be compiled as a single file. Expect this to be fixed shortly.

## Roadmap

A backport to Scala 2 and an adaptation layer to ease migration from shapeless
2.x to shapeless 3.x are in progress. Other components of shapeless 2 will be
migrated to shapeless 3 as it evolves.

## Finding out more about the project

shapeless is part of the [Typelevel][typelevel] family of projects. It is an
Open Source project under the Apache License v2, hosted on [github][source].
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
