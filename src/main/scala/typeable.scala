trait Typeable[U] {
  def cast(t : Any) : Option[U]
}

trait LowPriorityTypeable {
  implicit def dfltTypeable[U](implicit mU : ClassManifest[U]) = new Typeable[U] {
    def cast(t : Any) : Option[U] = {
      if(t == null || (mU.erasure isAssignableFrom t.getClass)) Some(t.asInstanceOf[U]) else None
    }
  }
}

object Typeable extends LowPriorityTypeable {
  import java.{ lang => jl }
  import scala.collection.GenTraversable
  import HList._
  
  class Cast(t : Any) {
    def cast[U](implicit castU : Typeable[U]) = castU.cast(t)
  }
  
  implicit def anyCast(t : Any) : Cast = new Cast(t)

  case class ValueTypeable[U, B](cB : Class[B]) extends Typeable[U] {
    def cast(t : Any) : Option[U] = {
      if(t == null || (cB isAssignableFrom t.getClass)) Some(t.asInstanceOf[U]) else None
    }
  }
  
  implicit val byteTypeable = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte])
  implicit val shortTypeable = ValueTypeable[Short, jl.Short](classOf[jl.Short])
  implicit val charTypeable = ValueTypeable[Char, jl.Character](classOf[jl.Character])
  implicit val intTypeable = ValueTypeable[Int, jl.Integer](classOf[jl.Integer])
  implicit val longTypeable = ValueTypeable[Long, jl.Long](classOf[jl.Long])
  implicit val floatTypeable = ValueTypeable[Float, jl.Float](classOf[jl.Float])
  implicit val doubleTypeable = ValueTypeable[Double, jl.Double](classOf[jl.Double])
  implicit val booleanTypeable = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean])
  implicit val unitTypeable = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit])
  
  def isValClass[T](clazz : Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]
  
  implicit val anyValTypeable = new Typeable[AnyVal] {
    def cast(t : Any) : Option[AnyVal] = {
      if(t == null || isValClass(t.getClass)) Some(t.asInstanceOf[AnyVal]) else None
    }
  }

  implicit val anyRefTypeable = new Typeable[AnyRef] {
    def cast(t : Any) : Option[AnyRef] = {
      if(t != null && isValClass(t.getClass)) None else Some(t.asInstanceOf[AnyRef])
    }
  }
  
  implicit def optionTypeable[T](implicit castT : Typeable[T]) = new Typeable[Option[T]] {
    def cast(t : Any) : Option[Option[T]] = {
      if(t == null) Some(t.asInstanceOf[Option[T]])
      else if(t.isInstanceOf[Option[_]]) {
        val o = t.asInstanceOf[Option[_]]
        if(o.isEmpty) Some(t.asInstanceOf[Option[T]])
        else for(e <- o; _ <- e.cast[T]) yield t.asInstanceOf[Option[T]]
      } else None
    }
  }
  
  implicit def eitherTypeable[A, B](implicit castA : Typeable[Left[A, B]], castB : Typeable[Right[A, B]]) = new Typeable[Either[A, B]] {
    def cast(t : Any) : Option[Either[A, B]] = {
      t.cast[Left[A, B]] orElse t.cast[Right[A, B]]
    }
  }

  implicit def leftTypeable[A, B](implicit castA : Typeable[A]) = new Typeable[Left[A, B]] {
    def cast(t : Any) : Option[Left[A, B]] = {
      if(t == null) Some(t.asInstanceOf[Left[A, B]])
      else if(t.isInstanceOf[Left[_, _]]) {
        val l = t.asInstanceOf[Left[_, _]]
        for(a <- l.a.cast[A]) yield t.asInstanceOf[Left[A, B]]
      } else None
    }
  }

  implicit def rightTypeable[A, B](implicit castB : Typeable[B]) = new Typeable[Right[A, B]] {
    def cast(t : Any) : Option[Right[A, B]] = {
      if(t == null) Some(t.asInstanceOf[Right[A, B]])
      else if(t.isInstanceOf[Right[_, _]]) {
        val r = t.asInstanceOf[Right[_, _]]
        for(b <- r.b.cast[B]) yield t.asInstanceOf[Right[A, B]]
      } else None
    }
  }

  implicit def genTraversableTypeable[CC[X] <: GenTraversable[X], T]
    (implicit mCC : ClassManifest[CC[_]], castT : Typeable[T]) = new Typeable[CC[T]] {
    def cast(t : Any) : Option[CC[T]] =
      if(t == null) Some(t.asInstanceOf[CC[T]])
      else if(mCC.erasure isAssignableFrom t.getClass) {
        val cc = t.asInstanceOf[CC[Any]]
        if(cc.forall(_.cast[T].isDefined)) Some(t.asInstanceOf[CC[T]])
        else None
      } else None
  }
  
  implicit def genMapTypeable[M[X, Y] <: Map[X, Y], T, U]
    (implicit mM : ClassManifest[M[_, _]], castTU : Typeable[(T, U)]) = new Typeable[Map[T, U]] {
    def cast(t : Any) : Option[M[T, U]] =
      if(t == null) Some(t.asInstanceOf[M[T, U]])
      else if(mM.erasure isAssignableFrom t.getClass) {
        val m = t.asInstanceOf[M[Any, Any]]
        if(m.forall(_.cast[(T, U)].isDefined)) Some(t.asInstanceOf[M[T, U]])
        else None
      } else None
  }
  
  implicit def hnilTypeable = new Typeable[HNil] {
    def cast(t : Any) : Option[HNil] = if(t == null || t.isInstanceOf[HNil]) Some(t.asInstanceOf[HNil]) else None
  }
  
  implicit def hlistTypeable[H, T <: HList](implicit castH : Typeable[H], castT : Typeable[T]) = new Typeable[H :: T] {
    def cast(t : Any) : Option[H :: T] = {
      if(t == null) Some(t.asInstanceOf[H :: T])
      else if(t.isInstanceOf[HCons[_, _ <: HList]]) {
        val l = t.asInstanceOf[HCons[_, _ <: HList]]
        for(hd <- l.head.cast[H]; tl <- (l.tail : Any).cast[T]) yield t.asInstanceOf[H :: T]
      } else None
    }
  }
  
  implicit def tuple2Typeable[A, B](implicit castA : Typeable[A], castB : Typeable[B]) = new Typeable[(A, B)] {
    def cast(t : Any) : Option[(A, B)] = {
      if(t == null) Some(t.asInstanceOf[(A, B)])
      else if(t.isInstanceOf[(_, _)]) {
        val p = t.asInstanceOf[(_, _)]
        for(a <- p._1.cast[A]; b <- p._2.cast[B]) yield t.asInstanceOf[(A, B)]
      } else None
    }
  }
}
