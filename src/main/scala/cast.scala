abstract class Castable[U] {
  val manifest : ClassManifest[_]
  def cast(t : Any) : Option[U] = {
    val mT = ClassManifest.fromClass(t.getClass)
    if(mT <:< manifest) Some(t.asInstanceOf[U]) else None
  }
}

trait LowPriorityCastable {
  def mkCastable[U, B : ClassManifest] = new Castable[U] {
    val manifest = implicitly[ClassManifest[B]]
  }

  implicit def dfltCastable[T : ClassManifest] = mkCastable[T, T]
}

object Castable extends LowPriorityCastable {
  import java.{ lang => jl }
  
  implicit val byteBCM = mkCastable[Byte, jl.Byte]
  implicit val shortBCM = mkCastable[Short, jl.Short]
  implicit val charBCM = mkCastable[Char, jl.Character]
  implicit val intBCM = mkCastable[Int, jl.Integer]
  implicit val longBCM = mkCastable[Long, jl.Long]
  implicit val floatBCM = mkCastable[Float, jl.Float]
  implicit val doubleBCM = mkCastable[Double, jl.Double]
  implicit val booleanBCM = mkCastable[Boolean, jl.Boolean]
  implicit val unitBCM = mkCastable[Unit, jl.Void]
  
  class CastFrom(t : Any) {
    def cast[U](implicit mU : Castable[U]) = mU.cast(t)
  }

  implicit def anyCastable(t : Any) : CastFrom = new CastFrom(t)
  
  def main(args : Array[String]) {
    val i : Any = 23
    val j : Int = i.cast[Int].get
  }
}
