# shapeless: generic programming for Scala

**shapeless** is a type class and dependent type based generic programming library for Scala. It had its origins in
several talks by Miles Sabin ([@milessabin][milessabin]), given over the course of 2011, on implementing [scrap your
boilerplate][syb] and [higher rank polymorphism][higherrank] in Scala. Since then it has evolved from being a resolutely
experimental project into library which, while still testing the limits of what's possible in Scala, is being used
widely in production systems wherever there are arities to be abstracted over and boilerplate to be scrapped. 

[![Build Status](https://api.travis-ci.org/milessabin/shapeless.png?branch=scala-2.10.x)](https://travis-ci.org/milessabin/shapeless)
[![Stories in Ready](https://badge.waffle.io/milessabin/shapeless.png?label=Ready)](https://waffle.io/milessabin/shapeless)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/shapeless)

## Finding out more about the project

A feature overview of shapeless-2.0.0 can be found [here][features200]. If you are upgrading from shapeless-1.2.4 you
will find the [release notes][relnotes] and [migration guide][migration] useful.

shapeless is part of the [typelevel][] family of projects. It is an Open Source project under the Apache License v2,
hosted on [github][source]. Binary artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype]
and synced to Maven Central.

There is a [mailing list][group] for discussion around generic programming in Scala in general and shapeless in
particular. You will also find many of the main shapeless contributors on IRC in the #shapeless channel on
[freenode][irc]. Questions about shapeless are often asked and answered under the [shapeless tag on StackOverflow][so].
Some articles on the implementation techniques can be found on [Miles's blog][blog], and Olivera, Moors and Odersky,
[Type Classes as Object and Implicits][tcoi] is useful background material.

Support for Scala 2.9.x is still available via the shapeless-1.2.4 release (feature overview [here][features124]). It
isn't straightforward to bring the latest shapeless features to Scala versions which don't support implicit macros, and
this release should be treated as a stopgap until you are able to move your project to Scala 2.11. It might, however, be
feasible to backport some of the updates via a compiler plugin for Scala 2.9.x, and anyone interested in contributing or
sponsoring such work should [get in touch](mailto:miles@milessabin.com).

[features200]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0
[features124]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-1.2.4
[relnotes]: https://github.com/milessabin/shapeless/wiki/Release-notes:-shapeless-2.0.0
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

## Participation

The shapeless project supports the [Typelevel][typelevel] [code of conduct][codeofconduct] and wants all of its
channels (mailing list, IRC, github, etc.) to be welcoming environments for everyone.

Whilst shapeless is a somewhat "advanced" Scala library, it is a lot more approachable than many people think.
Contributors are usually available to field questions, give advice and discuss ideas on [#shapeless][irc] and
the [mailing list][group], and for people wanting to take their first steps at contributing we have a selection
of open issues flagged up as being [good candidates to take on][lowhangingfruit]. No contribution is too small,
and guidance is always available.

[codeofconduct]: http://typelevel.org/conduct.html
[lowhangingfruit]: https://github.com/milessabin/shapeless/issues?q=is%3Aopen+is%3Aissue+label%3A%22Low+hanging+fruit%22

## Using shapeless

Binary release artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype] and synced to Maven
Central. Snapshots of the master and scala-2.10.x branches are built using [Travis CI][ci] and automatically published
to the Sonatype OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add,

```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
```

Please be aware that SBT 0.13.6 has an [issue][namehashing] related to its new name hashing feature which when
compiling with shapeless might cause SBT to loop indefinitely consuming all heap. Workarounds are to move to an
earlier (0.13.5) or later (0.13.7) SBT version or disable name hashing by adding,

```scala
incOptions := incOptions.value.withNameHashing(false)
```

to your settings.

[ci]: https://travis-ci.org/milessabin/shapeless

### shapeless-2.0.0

Builds are available for Scala 2.11.0 and later and for Scala 2.10.4.

```scala
// For Scala 2.11.4
scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.0.0"
)
```

For Scala 2.10.x you must specify a Scala version of at least 2.10.2, and add either `cross CrossVersion.full` or
provide an explicit Scala version suffix to your shapeless dependency,

```scala
// For Scala 2.10.x >= 2.10.2
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.4" % "2.0.0"
  // "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full  // Alternatively ...
)
```

Note that Scala 2.10.x releases are compatible with each other starting from 2.10.2, so a mismatch in minor versions
above would be fine.

If your project is built with Scala 2.10.4 and Scala 2.11.0 and later, then you will need to add the following,

```scala
val shapeless = Def setting (
    CrossVersion partialVersion scalaVersion.value match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 => 
      "com.chuusai" %% "shapeless" % "2.0.0"
    case Some((2, 10)) => 
      "com.chuusai" %  "shapeless" % "2.0.0" cross CrossVersion.full
  }
)

libraryDependencies ++= Seq(
  shapeless.value
)
```

This is needed because the shapeless binaries for Scala 2.10 includes the Scala bug fix digit. There specific binaries for 2.10.2, 2.10.3 and 2.10.4, while in the Scala 2.11 series the bug fix digit is omitted.
```scala
"com.chuusai" %  "shapeless_2.10.2" % "2.0.0"

"com.chuusai" %  "shapeless_2.10.3" % "2.0.0"

"com.chuusai" %  "shapeless_2.10.4" % "2.0.0"

"com.chuusai" %  "shapeless_2.11" % "2.0.0"
```




### shapeless-2.1.0-SNAPSHOT

Builds are available for Scala 2.11.0 and later and for Scala 2.10.4. The main line of development for
shapeless 2.1.0 will be Scala 2.11.4 with Scala 2.10.x supported via the macro paradise compiler plugin.

```scala
scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.1.0-SNAPSHOT" changing()
)
```

Note that for Scala 2.10.4 you must provide an explicit Scala version suffix to your shapeless dependency, and it is recommanded to add the macro paradise plugin to your build, for some macros provided by shapeless to work smoothly,

```scala
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.4" % "2.1.0-SNAPSHOT" changing(),
  compilerPlugin("org.scalamacros" % "paradise_2.10.4" % "2.0.1")
)
```

### shapeless-1.2.4

Builds are available for Scala 2.9, 2.10 and 2.11. If you are working with Scala 2.10.2 or later you should use
shapeless-2.0.0 instead.

If your project is built with Scala 2.9.3 or earlier, then you will need to specify the `-Ydependent-method-types`
compiler flag,

```scala
scalaVersion := "2.9.3"

scalacOptions += "-Ydependent-method-types"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

This option isn't necessary or supported in Scala 2.10 and later, so you should omit it if you are building with
Scala 2.10.2 or later,

```scala
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

If you want to be able to support building relative to both 2.9.3 and 2.10 and later then you should use the 2.10.4
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

shapeless is built with 0.13.7 or later. SBT 0.13.6 has an [issue][namehashing] related to its new name hashing
feature which when compiling shapeless causes SBT to loop indefinitely consuming all heap. Workarounds are to move to
an earlier or later SBT version or disable name hashing by adding,

```scala
incOptions := incOptions.value.withNameHashing(false)
```

to your settings.

The master of shapeless branch is built with Scala 2.11.4 by default. To build with Scala 2.10.x you should check out
the scala-2.10.x branch. As a general rule all new features and bugfixes are made against master and Scala 2.11.4 and
merged into the scala-2.10.x branch with only the minimal changes needed for forwards compatibility.

[namehashing]: https://github.com/sbt/sbt/issues/1640

## Contributors

+ Alexandre Archambault <alexandre.archambault@gmail.com> [@alxarchambault](https://twitter.com/alxarchambault)
+ Alois Cochard <alois.cochard@gmail.com> [@aloiscochard](https://twitter.com/aloiscochard)
+ Andrew Brett <github@bretts.org> [@Ephemerix](https://twitter.com/Ephemerix)
+ Ben Hutchison <brhutchison@gmail.com> [@ben_hutchison](https://twitter.com/ben_hutchison)
+ Ben James <ben.james@guardian.co.uk> [@bmjames](https://twitter.com/bmjames)
+ Brian McKenna <brian@brianmckenna.org> [@puffnfresh](https://twitter.com/puffnfresh)
+ Chris Hodapp <clhodapp1@gmail.com> [@clhodapp](https://twitter.com/clhodapp)
+ Cody Allen <ceedubs@gmail.com> [@fourierstrick](https://twitter.com/fourierstrick)
+ Dale Wijnand <dale.wijnand@gmail.com> [@dwijnand](https://twitter.com/dwijnand)
+ Dario Rexin <dario.rexin@r3-tech.de> [@evonox](https://twitter.com/evonox)
+ Eugene Burmako <xeno.by@gmail.com> [@xeno_by](https://twitter.com/xeno_by)
+ Filipe Nepomuceno <filinep@gmail.com
+ George Leontiev <folone@gmail.com> [@folone](https://twitter.com/folone)
+ Howard Branch <purestgreen@gmail.com> [@purestgreen](https://twitter.com/purestgreen)
+ Huw Giddens <hgiddens@gmail.com>
+ Jason Zaugg <jzaugg@gmail.com> [@retronym](https://twitter.com/retronym)
+ Jean-Remi Desjardins <jeanremi.desjardins@gmail.com> [@jrdesjardins](https://twitter.com/jrdesjardins)
+ Johannes Rudolph <johannes.rudolph@gmail.com> [@virtualvoid](https://twitter.com/virtualvoid)
+ Joni Freeman <joni.freeman@ri.fi> [@jonifreeman](https://twitter.com/jonifreeman)
+ Julien Tournay <boudhevil@gmail.com> [@skaalf](https://twitter.com/skaalf)
+ Jules Gosnell <jules_gosnell@yahoo.com
+ Kevin Wright <kev.lee.wright@gmail.com> [@thecoda](https://twitter.com/thecoda)
+ Lars Hupel <lars.hupel@mytum.de> [@larsr_h](https://twitter.com/larsr_h)
+ Mario Pastorelli <mario.pastorelli@teralytics.ch> [@mapastr](https://twitter.com/mapastr)
+ Mathias Doenitz <mathias@spray.io> [@sirthias](https://twitter.com/sirthias)
+ Michael Donaghy <md401@srcf.ucam.org>
+ Michael Pilquist <mpilquist@gmail.com> [@mpilquist](https://twitter.com/mpilquist)
+ Miles Sabin <miles@milessabin.com> [@milessabin](https://twitter.com/milessabin)
+ Nikolas Evangelopoulos <nikolas@jkl.gr>
+ Oleg Aleshko <olegych@tut.by> [@OlegYch](https://twitter.com/OlegYch)
+ Owein Reese <owreese@gmail.com> [@OweinReese](https://twitter.com/OweinReese)
+ Paolo G. Giarrusso <p.giarrusso@gmail.com> [@blaisorblade](https://twitter.com/blaisorblade)
+ Pascal Voitot <pascal.voitot.dev@gmail.com> [@mandubian](https://twitter.com/mandubian)
+ Renato Cavalcanti <renato@strongtyped.io> [@renatocaval](https://twitter.com/renatocaval)
+ Sam Halliday <sam.halliday@gmail.com> [@fommil](https://twitter.com/fommil)
+ Sarah Gerweck <sarah.a180@gmail.com> [@SGerweck](https://twitter.com/SGerweck)
+ Simon Hafner <hafnersimon@gmail.com> [@reactormonk](https://twitter.com/reactormonk)
+ Stacy Curl <stacy.curl@gmail.com> [@stacycurl](https://twitter.com/stacycurl)
+ Stephen Compall <scompall@nocandysw.com> [@S11001001](https://twitter.com/S11001001)
+ Tom Switzer <thomas.switzer@gmail.com> [@tixxit](https://twitter.com/tixxit)
+ Travis Brown <travisrobertbrown@gmail.com> [@travisbrown](https://twitter.com/travisbrown)
