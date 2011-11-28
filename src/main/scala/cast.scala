trait Castable[U] {
  def cast(t : Any) : Option[U]
}

trait LowPriorityCastable {
  implicit def dfltCastable[U](implicit mU : ClassManifest[U]) = new Castable[U] {
    def cast(t : Any) : Option[U] = {
      if(t == null || (mU.erasure isAssignableFrom t.getClass)) Some(t.asInstanceOf[U]) else None
    }
  }
}

object Castable extends LowPriorityCastable {
  import java.{ lang => jl }
  import scala.collection.GenTraversable
  import HList._
  
  class CastFrom(t : Any) {
    def cast[U](implicit castU : Castable[U]) = castU.cast(t)
  }
  
  implicit def anyCastable(t : Any) : CastFrom = new CastFrom(t)

  case class ValueCastable[U, B](cB : Class[B]) extends Castable[U] {
    def cast(t : Any) : Option[U] = {
      if(t == null || (cB isAssignableFrom t.getClass)) Some(t.asInstanceOf[U]) else None
    }
  }
  
  implicit val byteCastable = ValueCastable[Byte, jl.Byte](classOf[jl.Byte])
  implicit val shortCastable = ValueCastable[Short, jl.Short](classOf[jl.Short])
  implicit val charCastable = ValueCastable[Char, jl.Character](classOf[jl.Character])
  implicit val intCastable = ValueCastable[Int, jl.Integer](classOf[jl.Integer])
  implicit val longCastable = ValueCastable[Long, jl.Long](classOf[jl.Long])
  implicit val floatCastable = ValueCastable[Float, jl.Float](classOf[jl.Float])
  implicit val doubleCastable = ValueCastable[Double, jl.Double](classOf[jl.Double])
  implicit val booleanCastable = ValueCastable[Boolean, jl.Boolean](classOf[jl.Boolean])
  implicit val unitCastable = ValueCastable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit])
  
  def isValClass[T](clazz : Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]
  
  implicit val anyValCastable = new Castable[AnyVal] {
    def cast(t : Any) : Option[AnyVal] = {
      if(t == null || isValClass(t.getClass)) Some(t.asInstanceOf[AnyVal]) else None
    }
  }

  implicit val anyRefCastable = new Castable[AnyRef] {
    def cast(t : Any) : Option[AnyRef] = {
      if(t != null && isValClass(t.getClass)) None else Some(t.asInstanceOf[AnyRef])
    }
  }
  
  implicit def optionCastable[T](implicit castT : Castable[T]) = new Castable[Option[T]] {
    def cast(t : Any) : Option[Option[T]] = {
      if(t == null) Some(t.asInstanceOf[Option[T]])
      else if(t.isInstanceOf[Option[_]]) {
        val o = t.asInstanceOf[Option[_]]
        if(o.isEmpty) Some(t.asInstanceOf[Option[T]])
        else for(e <- o; _ <- e.cast[T]) yield t.asInstanceOf[Option[T]]
      } else None
    }
  }
  
  implicit def eitherCastable[A, B](implicit castA : Castable[Left[A, B]], castB : Castable[Right[A, B]]) = new Castable[Either[A, B]] {
    def cast(t : Any) : Option[Either[A, B]] = {
      t.cast[Left[A, B]] orElse t.cast[Right[A, B]]
    }
  }

  implicit def leftCastable[A, B](implicit castA : Castable[A]) = new Castable[Left[A, B]] {
    def cast(t : Any) : Option[Left[A, B]] = {
      if(t == null) Some(t.asInstanceOf[Left[A, B]])
      else if(t.isInstanceOf[Left[_, _]]) {
        val l = t.asInstanceOf[Left[_, _]]
        for(a <- l.a.cast[A]) yield t.asInstanceOf[Left[A, B]]
      } else None
    }
  }

  implicit def rightCastable[A, B](implicit castB : Castable[B]) = new Castable[Right[A, B]] {
    def cast(t : Any) : Option[Right[A, B]] = {
      if(t == null) Some(t.asInstanceOf[Right[A, B]])
      else if(t.isInstanceOf[Right[_, _]]) {
        val r = t.asInstanceOf[Right[_, _]]
        for(b <- r.b.cast[B]) yield t.asInstanceOf[Right[A, B]]
      } else None
    }
  }

  implicit def genTraversableCastable[CC[X] <: GenTraversable[X], T]
    (implicit mCC : ClassManifest[CC[_]], castT : Castable[T]) = new Castable[CC[T]] {
    def cast(t : Any) : Option[CC[T]] =
      if(t == null) Some(t.asInstanceOf[CC[T]])
      else if(mCC.erasure isAssignableFrom t.getClass) {
        val cc = t.asInstanceOf[CC[Any]]
        if(cc.forall(_.cast[T].isDefined)) Some(t.asInstanceOf[CC[T]])
        else None
      } else None
  }
  
  implicit def genMapCastable[M[X, Y] <: Map[X, Y], T, U]
    (implicit mM : ClassManifest[M[_, _]], castTU : Castable[(T, U)]) = new Castable[Map[T, U]] {
    def cast(t : Any) : Option[M[T, U]] =
      if(t == null) Some(t.asInstanceOf[M[T, U]])
      else if(mM.erasure isAssignableFrom t.getClass) {
        val m = t.asInstanceOf[M[Any, Any]]
        if(m.forall(_.cast[(T, U)].isDefined)) Some(t.asInstanceOf[M[T, U]])
        else None
      } else None
  }
  
  implicit def hnilCastable = new Castable[HNil] {
    def cast(t : Any) : Option[HNil] = if(t == null || t.isInstanceOf[HNil]) Some(t.asInstanceOf[HNil]) else None
  }
  
  implicit def hlistCastable[H, T <: HList](implicit castH : Castable[H], castT : Castable[T]) = new Castable[H :: T] {
    def cast(t : Any) : Option[H :: T] = {
      if(t == null) Some(t.asInstanceOf[H :: T])
      else if(t.isInstanceOf[HCons[_, _ <: HList]]) {
        val l = t.asInstanceOf[HCons[_, _ <: HList]]
        for(hd <- l.head.cast[H]; tl <- (l.tail : Any).cast[T]) yield t.asInstanceOf[H :: T]
      } else None
    }
  }
  
  implicit def tuple2Castable[A, B](implicit castA : Castable[A], castB : Castable[B]) = new Castable[(A, B)] {
    def cast(t : Any) : Option[(A, B)] = {
      if(t == null) Some(t.asInstanceOf[(A, B)])
      else if(t.isInstanceOf[(_, _)]) {
        val p = t.asInstanceOf[(_, _)]
        for(a <- p._1.cast[A]; b <- p._2.cast[B]) yield t.asInstanceOf[(A, B)]
      } else None
    }
  }
}
