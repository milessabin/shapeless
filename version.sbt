
import org.typelevel.sbt.ReleaseSeries
import org.typelevel.sbt.Version._

TypelevelKeys.series in ThisBuild := ReleaseSeries(2,2)

TypelevelKeys.relativeVersion in ThisBuild := Relative(2,Snapshot)
