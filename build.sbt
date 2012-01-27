name := "shapeless"

organization := "com.chuusai"

version := "1.1.0"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions <++= scalaVersion map { version =>
  val Version = """(\d+)\.(\d+)\..*"""r
  val Version(major0, minor0) = version map identity
  val (major, minor) = (major0.toInt, minor0.toInt)
  if (major < 2 || (major == 2 && minor < 10)) 
  	Seq("-Ydependent-method-types")
 	else Nil
}

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.7" % "test"
)
