/*
 * Copyright (c) 2011 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

/**
 * Type class supporting type safe cast.
 * 
 * @author Miles Sabin
 */
trait Typeable[U] {
  def cast(t : Any) : Option[U]
}

trait LowPriorityTypeable {
  /**
   * Default `Typeable` instance. Note that this is safe only up to erasure.
   */
  implicit def dfltTypeable[U](implicit mU : ClassManifest[U]) = new Typeable[U] {
    def cast(t : Any) : Option[U] = {
      if(t == null || (mU.erasure isAssignableFrom t.getClass)) Some(t.asInstanceOf[U]) else None
    }
  }
}

/**
 * Provides instances of `Typeable`. Also provides an implicit conversion which enhances arbitrary values with a
 * `cast[U]` method.
 */
object Typeable extends TupleTypeableInstances with LowPriorityTypeable {
  import java.{ lang => jl }
  import scala.collection.{ GenMap, GenTraversable }
  
  class Cast(t : Any) {
    /**
     * Cast the receiver to a value of type `U` if possible. This operation will be as precise wrt erasure as possible
     * given the in-scope `Typeable` instances available.
     */
    def cast[U](implicit castU : Typeable[U]) = castU.cast(t)
  }
  
  implicit def anyCast(t : Any) : Cast = new Cast(t)

  case class ValueTypeable[U, B](cB : Class[B]) extends Typeable[U] {
    def cast(t : Any) : Option[U] = {
      if(t == null || (cB isAssignableFrom t.getClass)) Some(t.asInstanceOf[U]) else None
    }
  }
  
  /** Typeable instance for `Byte`. */
  implicit val byteTypeable = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte])
  /** Typeable instance for `Short`. */
  implicit val shortTypeable = ValueTypeable[Short, jl.Short](classOf[jl.Short])
  /** Typeable instance for `Char`. */
  implicit val charTypeable = ValueTypeable[Char, jl.Character](classOf[jl.Character])
  /** Typeable instance for `Int`. */
  implicit val intTypeable = ValueTypeable[Int, jl.Integer](classOf[jl.Integer])
  /** Typeable instance for `Long`. */
  implicit val longTypeable = ValueTypeable[Long, jl.Long](classOf[jl.Long])
  /** Typeable instance for `Float`. */
  implicit val floatTypeable = ValueTypeable[Float, jl.Float](classOf[jl.Float])
  /** Typeable instance for `Double`. */
  implicit val doubleTypeable = ValueTypeable[Double, jl.Double](classOf[jl.Double])
  /** Typeable instance for `Boolean`. */
  implicit val booleanTypeable = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean])
  /** Typeable instance for `Unit`. */
  implicit val unitTypeable = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit])
  
  def isValClass[T](clazz : Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]
  
  /** Typeable instance for `AnyVal`. */
  implicit val anyValTypeable = new Typeable[AnyVal] {
    def cast(t : Any) : Option[AnyVal] = {
      if(t == null || isValClass(t.getClass)) Some(t.asInstanceOf[AnyVal]) else None
    }
  }

  /** Typeable instance for `AnyRef`. */
  implicit val anyRefTypeable = new Typeable[AnyRef] {
    def cast(t : Any) : Option[AnyRef] = {
      if(t != null && isValClass(t.getClass)) None else Some(t.asInstanceOf[AnyRef])
    }
  }
  
  /** Typeable instance for `Option`. */
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
  
  /** Typeable instance for `Either`. */
  implicit def eitherTypeable[A, B](implicit castA : Typeable[Left[A, B]], castB : Typeable[Right[A, B]]) =
    new Typeable[Either[A, B]] {
      def cast(t : Any) : Option[Either[A, B]] = {
        t.cast[Left[A, B]] orElse t.cast[Right[A, B]]
      }
    }

  /** Typeable instance for `Left`. */
  implicit def leftTypeable[A, B](implicit castA : Typeable[A]) = new Typeable[Left[A, B]] {
    def cast(t : Any) : Option[Left[A, B]] = {
      if(t == null) Some(t.asInstanceOf[Left[A, B]])
      else if(t.isInstanceOf[Left[_, _]]) {
        val l = t.asInstanceOf[Left[_, _]]
        for(a <- l.a.cast[A]) yield t.asInstanceOf[Left[A, B]]
      } else None
    }
  }

  /** Typeable instance for `Right`. */
  implicit def rightTypeable[A, B](implicit castB : Typeable[B]) = new Typeable[Right[A, B]] {
    def cast(t : Any) : Option[Right[A, B]] = {
      if(t == null) Some(t.asInstanceOf[Right[A, B]])
      else if(t.isInstanceOf[Right[_, _]]) {
        val r = t.asInstanceOf[Right[_, _]]
        for(b <- r.b.cast[B]) yield t.asInstanceOf[Right[A, B]]
      } else None
    }
  }

  /** Typeable instance for `GenTraversable`. Note that the contents will tested for conformance to the element type. */
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
  
  /** Typeable instance for `Map`. Note that the contents will tested for conformance to the key/value types. */
  implicit def genMapTypeable[M[X, Y], T, U]  // (Temporary?) workaround for inference issue with 2.10.0 ~M3 
    (implicit ev : M[T, U] <:< GenMap[T, U], mM : ClassManifest[M[_, _]], castTU : Typeable[(T, U)]) =
      new Typeable[M[T, U]] {
        def cast(t : Any) : Option[M[T, U]] =
          if(t == null) Some(t.asInstanceOf[M[T, U]])
          else if(mM.erasure isAssignableFrom t.getClass) {
            val m = t.asInstanceOf[GenMap[Any, Any]]
            if(m.forall(_.cast[(T, U)].isDefined)) Some(t.asInstanceOf[M[T, U]])
            else None
          } else None
  }
  
  /** Typeable instance for `HNil`. */
  implicit def hnilTypeable = new Typeable[HNil] {
    def cast(t : Any) : Option[HNil] = if(t == null || t.isInstanceOf[HNil]) Some(t.asInstanceOf[HNil]) else None
  }
  
  /** Typeable instance for `HList`s. Note that the contents will tested for conformance to the element types. */
  implicit def hlistTypeable[H, T <: HList](implicit castH : Typeable[H], castT : Typeable[T]) = new Typeable[H :: T] {
    def cast(t : Any) : Option[H :: T] = {
      if(t == null) Some(t.asInstanceOf[H :: T])
      else if(t.isInstanceOf[::[_, _ <: HList]]) {
        val l = t.asInstanceOf[::[_, _ <: HList]]
        for(hd <- l.head.cast[H]; tl <- (l.tail : Any).cast[T]) yield t.asInstanceOf[H :: T]
      } else None
    }
  }
}