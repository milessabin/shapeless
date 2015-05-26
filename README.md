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

A full feature overview of shapeless-2.1.0 is in preparation. In the meantime, please refer to the
[release notes][relnotes210] and the feature overview for shapeless-2.0.0 which can be found [here][features200]. If you
are upgrading from shapeless-2.0.0 you will find the [migration guide][migration210] useful.

shapeless is part of the [Typelevel][typelevel] family of projects. It is an Open Source project under the Apache
License v2, hosted on [github][source]. Binary artefacts are published to the
[Sonatype OSS Repository Hosting service][sonatype] and synced to Maven Central.

Discussion of shapeless and generic programming in Scala in general happens on the [Typelevel mailing list][group].
You will also find many of the main shapeless contributors in its [Gitter channel][gitter] and on IRC in
the #shapeless channel on [freenode][irc]. Questions about shapeless are often asked and answered under the
[shapeless tag on StackOverflow][so]. Some articles on the implementation techniques can be found on
[Miles's blog][blog], and Olivera, Moors and Odersky, [Type Classes as Object and Implicits][tcoi] is useful
background material.

Support for Scala 2.9.x is still available via the shapeless-1.2.4 release (feature overview [here][features124]). It
isn't straightforward to bring the latest shapeless features to Scala versions which don't support implicit macros, and
this release should be treated as a stopgap until you are able to move your project to Scala 2.11. It might, however, be
feasible to backport some of the updates via a compiler plugin for Scala 2.9.x, and anyone interested in contributing or
sponsoring such work should [get in touch](mailto:miles@milessabin.com).

[features200]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0
[features124]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-1.2.4
[relnotes]: https://github.com/milessabin/shapeless/wiki/Release-notes:-shapeless-2.0.0
[relnotes210]: https://github.com/milessabin/shapeless/wiki/Release-notes:-shapeless-2.0.0
[migration]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-1.2.4-to-2.0.0 
[migration210]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-2.0.0-to-2.1.0 
[milessabin]: https://twitter.com/milessabin
[syb]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
[higherrank]: http://camlunity.ru/swap/ocaml/Sexy%20Types.pdf
[typelevel]: http://typelevel.org/
[scalaz]: https://github.com/scalaz/scalaz
[spire]: https://github.com/non/spire
[tcoi]: http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf
[source]: https://github.com/milessabin/shapeless
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~shapeless
[wiki]: https://github.com/milessabin/shapeless/wiki
[oldgroup]: https://groups.google.com/group/shapeless-dev
[group]: https://groups.google.com/group/typelevel
[so]: http://stackoverflow.com/questions/tagged/shapeless
[gitter]: https://gitter.im/milessabin/shapeless
[irc]: http://webchat.freenode.net?channels=%23shapeless
[blog]: http://www.chuusai.com/blog

## Participation

The shapeless project supports the [Typelevel][typelevel] [code of conduct][codeofconduct] and wants all of its
channels (mailing list, Gitter, IRC, github, etc.) to be welcoming environments for everyone.

Whilst shapeless is a somewhat "advanced" Scala library, it is a lot more approachable than many people think.
Contributors are usually available to field questions, give advice and discuss ideas on the [Gitter channel][gitter],
on [#shapeless][irc] and the [mailing list][group], and for people wanting to take their first steps at contributing
we have a selection of open issues flagged up as being [good candidates to take on][lowhangingfruit]. No contribution
is too small, and guidance is always available.

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

### shapeless-2.2.0

Builds are available for Scala 2.11.x and for Scala 2.10.x. The main line of development for
shapeless 2.2.0 is Scala 2.11.6 with Scala 2.10.x supported via the macro paradise compiler plugin.

```scala
scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.0"
)
```

If you are using Scala 2.10.x, note that unlike earlier versions, it is no longer necessary to provide an explicit
Scala version suffix for your shapeless dependency. You must however ensure that you are using Scala version 2.10.2
or greater, with Scala 2.10.5 (or switching to 2.11.x) strongly recommended. You should also add the macro paradise
plugin to your build,

```scala
scalaVersion := "2.10.5"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.0",
  compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
)
```

### Older releases

Please use a current release if possible. If unavoidable, you can find [usage information for older
releases][olderusage] on the shapeless wiki.

[olderusage]: https://github.com/milessabin/shapeless/wiki/Using-shapeless:-older-releases

## Building shapeless

shapeless is built with SBT 0.13.7 or later. SBT 0.13.6 has an [issue][namehashing] related to its new name hashing
feature which when compiling shapeless causes SBT to loop indefinitely consuming all heap. Workarounds are to move to
an earlier or later SBT version or disable name hashing by adding,

```scala
incOptions := incOptions.value.withNameHashing(false)
```

to your settings.

The master branch of shapeless is built with Scala 2.11.6 by default. To build with Scala 2.10.x you should check out
the scala-2.10.x branch. As a general rule all new features and bugfixes are made against master and Scala 2.11.6 and
merged into the scala-2.10.x branch with only the minimal changes needed for forwards compatibility.

[namehashing]: https://github.com/sbt/sbt/issues/1640

## Contributors

+ Alexander Konovalov <alex.knvl@gmail.com> [@alexknvl](https://twitter.com/alexknvl)
+ Alexandre Archambault <alexandre.archambault@gmail.com> [@alxarchambault](https://twitter.com/alxarchambault)
+ Alistair Johnson <alistair.johnson@johnsonusm.com>
+ Alois Cochard <alois.cochard@gmail.com> [@aloiscochard](https://twitter.com/aloiscochard)
+ Andrew Brett <github@bretts.org> [@Ephemerix](https://twitter.com/Ephemerix)
+ Ben Hutchison <brhutchison@gmail.com> [@ben_hutchison](https://twitter.com/ben_hutchison)
+ Ben James <ben.james@guardian.co.uk> [@bmjames](https://twitter.com/bmjames)
+ Brian McKenna <brian@brianmckenna.org> [@puffnfresh](https://twitter.com/puffnfresh)
+ Chris Hodapp <clhodapp1@gmail.com> [@clhodapp](https://twitter.com/clhodapp)
+ Cody Allen <ceedubs@gmail.com> [@fourierstrick](https://twitter.com/fourierstrick)
+ Dale Wijnand <dale.wijnand@gmail.com> [@dwijnand](https://twitter.com/dwijnand)
+ Dario Rexin <dario.rexin@r3-tech.de> [@evonox](https://twitter.com/evonox)
+ David Barri <japgolly@gmail.com> [@japgolly](https://twitter.com/japgolly)
+ Denis Mikhaylov <notxcain@gmail.com> [@notxcain](https://twitter.com/@notxcain)
+ Eugene Burmako <xeno.by@gmail.com> [@xeno_by](https://twitter.com/xeno_by)
+ Filipe Nepomuceno <filinep@gmail.com>
+ George Leontiev <folone@gmail.com> [@folone](https://twitter.com/folone)
+ Howard Branch <purestgreen@gmail.com> [@purestgreen](https://twitter.com/purestgreen)
+ Huw Giddens <hgiddens@gmail.com>
+ Jason Zaugg <jzaugg@gmail.com> [@retronym](https://twitter.com/retronym)
+ Jean-Remi Desjardins <jeanremi.desjardins@gmail.com> [@jrdesjardins](https://twitter.com/jrdesjardins)
+ Johannes Rudolph <johannes.rudolph@gmail.com> [@virtualvoid](https://twitter.com/virtualvoid)
+ Johnny Everson <khronnuz@gmail.com> [@johnny_everson](https://twitter.com/johnny_everson)
+ Joni Freeman <joni.freeman@ri.fi> [@jonifreeman](https://twitter.com/jonifreeman)
+ Julien Tournay <boudhevil@gmail.com> [@skaalf](https://twitter.com/skaalf)
+ Jules Gosnell <jules_gosnell@yahoo.com>
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
+ Sébastien Doeraene <sjrdoeraene@gmail.com> [@sjrdoeraene](https://twitter.com/sjrdoeraene)
+ Simon Hafner <hafnersimon@gmail.com> [@reactormonk](https://twitter.com/reactormonk)
+ Stacy Curl <stacy.curl@gmail.com> [@stacycurl](https://twitter.com/stacycurl)
+ Stephen Compall <scompall@nocandysw.com> [@S11001001](https://twitter.com/S11001001)
+ Tom Switzer <thomas.switzer@gmail.com> [@tixxit](https://twitter.com/tixxit)
+ Travis Brown <travisrobertbrown@gmail.com> [@travisbrown](https://twitter.com/travisbrown)
+ Vladimir Matveev <vladimir.matweev@gmail.com> [@netvlm](https://twitter.com/netvlm)
