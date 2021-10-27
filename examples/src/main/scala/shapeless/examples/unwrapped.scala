
/*
 * Copyright (c) 2016 Miles Sabin 
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

package shapeless.examples

/**
  * Using the Unwrapped typeclass to support serializing wrapper types in their
  * unwrapped form
  *
  * @author Chris Hodapp
  */
object UnwrappedExamples {
  import shapeless._
  import shapeless.labelled._

  // When working with data, it is useful to define types to help you keep track
  // of what's what. For example, maybe users in your system have a numeric
  // ID and two stringish attributes: a handle and an email address. That is,
  // rather than defining your users as `case class User(id: String, Email: String)`,
  // you might want to define them as:
  case class Email(stringValue: String) extends AnyVal
  case class UserHandle(stringValue: String) extends AnyVal
  case class UserId(intValue: Int) extends AnyVal
  case class User(id: UserId, handle: UserHandle, email: Email)

  // This way, you can easily keep track of what's an email address and what's
  // a handle as these things flow through your code. So far, so good. But... If
  // you're using Shapeless, there's a good chance that one of the things you're
  // doing with it is writing a system to automatically generate serialization
  // codecs. For example:
  trait Encode[-T] {
    def toJson(t: T): String = fields(t).map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}")
    def fields(t: T): Map[String, String]
  }

  object Encode {
    def instance[T](f: T => Map[String, String]): Encode[T] = f(_)

    implicit val encodeHNil: Encode[HNil] =
      instance(_ => Map.empty)

    implicit def encodeHCons[K <: String with Singleton, V, Rest <: HList](
      implicit key: ValueOf[K], encodeV: => EncodeValue[V], encodeRest: Encode[Rest]
    ): Encode[FieldType[K, V] :: Rest] = instance { case h :: t =>
      encodeRest.fields(t) + (key.value -> encodeV.toJsonFragment(h))
    }

    // the magic one!
    implicit def encodeGeneric[T, Repr](
      implicit gen: LabelledGeneric.Aux[T, Repr], encodeRepr: => Encode[Repr]
    ): Encode[T] = instance { value =>
      encodeRepr.fields(gen.to(value))
    }
  }

  trait EncodeValue[-T] {
    def toJsonFragment(t: T): String
  }

  object EncodeValue {
    def instance[T](f: T => String): EncodeValue[T] = f(_)

    implicit val encodeString: EncodeValue[String] =
      instance(str => s""""$str"""")

    implicit val encodeInt: EncodeValue[Int] =
      instance(_.toString)

    implicit def encodeRoot[T](implicit r: => Encode[T]): EncodeValue[T] =
      instance(r.toJson)
  }

  // OK! Yay! Let's try it out!
  val user = User(UserId(1234), UserHandle("jeffe"), Email("jeff@email.com"))
  val encoder = the[Encode[User]]
  println(encoder.toJson(user))
  // {"email":{"stringValue":"jeff@email.com"},"handle":{"stringValue":"jeffe"},"id":{"intValue":1234}}

  // Ah! Our wrapper types are bleeding into our serialized JSON! Well, I
  // I guess maybe wrapper types aren't practical after all... Or maybe it's
  // shapeless-generated codecs that aren't practical?!? Just kidding, everything's
  // fine. We just need to use the Unwrapped typeclass in our serializers to cut
  // through the wrapper types:

  trait Encode2[-T] {
    def toJson(t: T): String = fields(t).map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}")
    def fields(t: T): Map[String, String]
  }

  object Encode2 {
    def instance[T](f: T => Map[String, String]): Encode2[T] = f(_)

    implicit val encodeHNil: Encode2[HNil] =
      instance(_ => Map.empty)

    implicit def encodeHCons[K <: String with Singleton, V, U, Rest <: HList](
      implicit
      key: ValueOf[K],
      uw: Unwrapped.Aux[V, U],
      encodeV: => EncodeValue[U],
      encodeRest: Encode2[Rest]
    ): Encode2[FieldType[K, V] :: Rest] = instance { case h :: t =>
      encodeRest.fields(t) + (key.value -> encodeV.toJsonFragment(uw.unwrap(h)))
    }

    implicit def encodeGeneric[T, Repr](
      implicit gen: LabelledGeneric.Aux[T, Repr], encodeRepr: => Encode2[Repr]
    ): Encode2[T] = instance { value =>
      encodeRepr.fields(gen.to(value))
    }
  }

  // OK! Let's try again
  val encoder2 = the[Encode2[User]]
  println(encoder2.toJson(user))
  // {"email":"jeff@email.com","handle":"jeffe","id":1234}

  // That's better.

  // Note that there are a few more places you'd probably want to insert unwrapping
  // if this was a real codec generator (basically, you'd likely want to remove
  // wrappers on objects too, not just wrappers on their fields)
}
