# shapeless: generic programming for Scala

**shapeless** is a type class and dependent type based generic programming library for Scala. It had its origins in
several talks by Miles Sabin ([@milessabin][milessabin]), given over the course of 2011, on implementing [scrap your
boilerplate][syb] and [higher rank polymorphism][higherrank] in Scala. Since then it has evolved from being a resolutely
experimental project into library which, while still testing the limits of what's possible in Scala, is being used
widely in production systems wherever there are arities to be abstracted over and boilerplate to be scrapped. 

## Finding out more about the project

A feature overview of shapeless-2.0.0 can be found [here][features200m1]. If you are upgrading from shapeless-1.2.4 you
will find the [release notes][relnotes] and [migration guide][migration] useful.

shapeless is part of the [typelevel][] family of projects along with [Scalaz][scalaz] and [Spire][spire]. It is an Open
Source project under the Apache License v2, hosted on [github][source]. Binary artefacts are published to the [Sonatype
OSS Repository Hosting service][sonatype] and synced to Maven Central.

The project is currently at the first milestone release of shapeless-2.0.0 and if you are starting to investigate
shapeless it is recommended that you start there: a final shapeless-2.0.0 release is expected before the end of 2013.
shapeless-2.0.0 takes advantage of the availability of implicit macros in Scala 2.10.2 to reduce, and in many cases
completely eliminate, the already minimal boilerplate that remained in earlier releases.

There is a [mailing list][group] for discussion around generic programming in Scala in general and shapeless in
particular. You will also find many of the main shapeless contributors on IRC in the #shapeless channel on
[freenode][irc]. Questions about shapeless are often asked and answered under the [shapeless tag on StackOverflow][so].
Some articles on the implementation techniques can be found on [Miles's blog][blog], and Olivera, Moors and Odersky,
[Type Classes as Object and Implicits][tcoi] is useful background material.

Support for Scala 2.9.x is still available via the shapeless-1.2.4 release (feature overview [here][features124]). It
isn't straightforward to bring the latest shapeless features to Scala versions which don't support implicit macros, and
this release should be treated as a stopgap until you are able to move your project to Scala 2.10. It might, however, be
feasible to backport some of the updates via a compiler plugin for Scala 2.9.x, and anyone interested in contributing or
sponsoring such work should [get in touch](mailto:miles@milessabin.com).

[features200m1]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0-M1
[features124]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-1.2.4
[relnotes]: https://github.com/milessabin/shapeless/wiki/Release-notes:-shapeless-2.0.0-M1
[migration]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-1.2.4-to-2.0.0 
[milessabin]: https://twitter.com/milessabin
[syb]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
[higherrank]: http://www.cs.rutgers.edu/~ccshan/cs252/usage.pdf
[typelevel]: http://typelevel.org/
[scalaz]: https://github.com/scalaz/scalaz
[spire]: https://github.com/non/spire
[tcoi]: http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf
[source]: https://github.com/milessabin/shapeless
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~shapeless
[wiki]: https://github.com/milessabin/shapeless/wiki
[group]: https://groups.google.com/group/shapeless-dev
[so]: http://stackoverflow.com/questions/tagged/shapeless
[irc]: http://freenode.net/
[blog]: http://www.chuusai.com/blog

## Using shapeless

Binary release artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype] and synced to Maven
Central. Snapshots of the master and scala-2.11.x branches are built using [Travis CI][ci] and automatically published
to the Sonatype OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add,

```scala
resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)
```

[ci]: https://travis-ci.org/milessabin/shapeless

### shapeless-2.0.0-M1

Builds are available for Scala 2.10.2 and later and Scala 2.11.0-M4. Note that you must specify a Scala version of at
least 2.10.2, and that you must add either `cross CrossVersion.full` or an explicit Scala version suffix to your
shapeless dependency,

```scala
scalaVersion := "2.10.2"
// scalaVersion := "2.11.0-M4" // alternatively ...

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
//  "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1" // alternatively ...
)
```


### shapeless-2.0.0-SNAPSHOT

Builds are available for Scala 2.10.2 and later and Scala 2.11.0-SNAPSHOT. Note that you must specify a Scala version of
at least 2.10.2, and that you must add either `cross CrossVersion.full` or an explicit Scala version suffix to your
shapeless dependency,

```scala
scalaVersion := "2.10.2"
// scalaVersion := "2.11.0-SNAPSHOT" // alternatively ...

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless" % "2.0.0-SNAPSHOT" cross CrossVersion.full changing()
//  "com.chuusai" % "shapeless_2.10.2" % "2.0.0-SNAPSHOT" changing() // alternatively ...
)
```

### shapeless-1.2.4

Builds are available for Scala 2.9 and 2.10. If you are working with Scala 2.10.2 or later you should use
shapeless-2.0.0-M1 instead.

If your project is built with Scala 2.9.3 or earlier, then you will need to specify the `-Ydependent-method-types`
compiler flag,

```scala
scalaVersion := "2.9.3"

scalacOptions += "-Ydependent-method-types"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

This option isn't necessary or supported in Scala 2.10, so you should omit it if you are building with Scala 2.10.2,

```scala
scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

If you want to be able to support building relative to both 2.9.3 and 2.10.2 then you should use the 2.10.2
configuration above and add the following,
 
```scala
scalacOptions <++= scalaVersion map { version =>
  val Some((major, minor)) = CrossVersion.partialVersion(version)
  if (major < 2 || (major == 2 && minor < 10)) 
    Seq("-Ydependent-method-types")
  else Nil
}
```

which will set the `-Ydependent-method-types` compiler flag conditionally on the actual Scala version in use.

## Building shapeless

shapeless is built with SBT 0.13.0. The master branch is built with Scala 2.10.2 by default. To build with Scala 2.11.0
you should check out the scala-2.11.x branch. As a general rule all new features and bugfixes are made against master
and Scala 2.10.2 and merged into the scala-2.11.x branch with only the minimal changes needed for forwards
compatibility.

## Contributors

+ Alois Cochard <alois.cochard@gmail.com> @aloiscochard
+ Ben Hutchison <brhutchison@gmail.com> @ben_hutchison
+ Ben James <ben.james@guardian.co.uk> @bmjames
+ Brian McKenna <brian@brianmckenna.org> @puffnfresh
+ Cody Allen <ceedubs@gmail.com>
+ George Leontiev <folone@gmail.com> @folone
+ Huw Giddens <hgiddens@gmail.com>
+ Jason Zaugg <jzaugg@gmail.com> @retronym
+ Joni Freeman <joni.freeman@ri.fi> @jonifreeman
+ Kevin Wright <kev.lee.wright@gmail.com> @thecoda
+ Lars Hupel <lars.hupel@mytum.de> @larsr_h
+ Mathias Doenitz <mathias@spray.io> @sirthias
+ Michael Donaghy <md401@srcf.ucam.org>
+ Michael Pilquist <mpilquist@gmail.com> @mpilquist
+ Miles Sabin <miles@milessabin.com> @milessabin
+ Nikolas Evangelopoulos <nikolas@jkl.gr> 
+ Stacy Curl <stacy.curl@gmail.com>
+ Tom Switzer <thomas.switzer@gmail.com> @tixxit
+ Travis Brown <travisrobertbrown@gmail.com> @travisbrown

