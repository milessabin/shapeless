trait LowPriorityCast {

  trait BoxedClassManifest[U] {
    val manifest : ClassManifest[_]
  }
  
  def bcm[U, B : ClassManifest] = new BoxedClassManifest[U] {
    val manifest = implicitly[ClassManifest[B]]
  }

  implicit def dfltBoxable[T : ClassManifest] = bcm[T, T]
}

object Cast extends LowPriorityCast {
  import java.{ lang => jl }
  
  implicit val byteBCM = bcm[Byte, jl.Byte]
  implicit val shortBCM = bcm[Short, jl.Short]
  implicit val charBCM = bcm[Char, jl.Character]
  implicit val intBCM = bcm[Int, jl.Integer]
  implicit val longBCM = bcm[Long, jl.Long]
  implicit val floatBCM = bcm[Float, jl.Float]
  implicit val doubleBCM = bcm[Double, jl.Double]
  implicit val booleanBCM = bcm[Boolean, jl.Boolean]
  implicit val unitBCM = bcm[Unit, jl.Void]
  
  class Castable(t : Any) {
    def cast[U : BoxedClassManifest] : Option[U] = {
      val mT = ClassManifest.fromClass(t.getClass)
      val mU = implicitly[BoxedClassManifest[U]].manifest
      if(mT <:< mU) Some(t.asInstanceOf[U]) else None
    }
  }

  implicit def anyCastable(t : Any) : Castable = new Castable(t)
  
  def main(args : Array[String]) {
    val i : Any = 23
    val j : Int = i.cast[Int].get
  }
}
