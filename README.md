# shapeless: generic programming for Scala

**shapeless** is a type class and dependent type based generic programming
library for Scala.

This branch contains an early preview of shapeless 3 for Dotty/Scala 3.

shapeless 3 was developed as part of a collaboration with Martin Odersky's
group at EPFL LAMP to develop [language-level support][mirror] for generic
programming for Scala 3 and is included in the [Dotty Community
Build][communitybuild].

Included so far is a full implementation of poly-kinded type class derivation
with the generality of shapeless, the compile time performance of Magnolia and
a significantly reduced binary footprint.

Support is provided for type classes indexed by types of kinds `*` (eg.
`Monoid`, `Eq`, `Show`), `* -> *` (eg. `Functor`, `Traverse`, `Monad`) `(* ->
*) -> *)` (eg. `FunctorK` aka `HFunctor` in Haskell) and `* -> * -> *` (eg.
`Bifunctor`). Support for additional kinds can be added faily straightforwardly
with a small amount of additional biolerplate for each kind. The first two of
these equal the power of shapeless 2 (see the [Kittens][kittens] type class
derivation library for [Cats][cats]), and the remainder go considerably beyond.

A backport to Scala 2, and an adaptation layer to ease migration from shapeless
2.x to shapeless 3.x are in progress. Other components of shapeless 2 will be
migrated to shapeless 3 as it evolves.

[![Build Status](https://api.travis-ci.org/milessabin/shapeless.png?branch=master)](https://travis-ci.org/milessabin/shapeless)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/shapeless)
[![Maven Central](https://img.shields.io/maven-central/v/com.chuusai/shapeless_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/com.chuusai/shapeless_2.13)

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
