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

package shapeless.examples

/**
 * Motivation for `Adjoin`: a router type that takes a path and optionally returns a value. `Adjoin`
 * provides a convenient way to compose routers into routers that return coproducts without nesting.
 *
 * @author Travis Brown
 */
object RouterExample extends App {
  import shapeless._, ops.adjoin._

  trait Router[A] { self =>
    def apply(path: String): Option[A]

    def map[B](f: A => B): Router[B] = new Router[B] {
      def apply(path: String): Option[B] = self(path).map(f)

      override def toString = self.toString
    }

    def orElse[B >: A](that: Router[B]): Router[B] = new Router[B] {
      def apply(path: String): Option[B] = self(path).orElse(that(path))

      override def toString = s"(${self.toString}|${that.toString})"
    }

    def :+:[B](that: Router[B])(implicit adjoin: Adjoin[B :+: A :+: CNil]): Router[adjoin.Out] =
      new Router[adjoin.Out] {
        def apply(path: String) = that(path).map(b => adjoin(Inl(b))).orElse(
          self(path).map(a => adjoin(Inr(Inl(a))))
        )
      }
  }

  def matchString(s: String): Router[String] = new Router[String] {
    def apply(path: String): Option[String] = if (path == s) Some(s) else None

    override def toString = s
  }

  val fooRouter: Router[Int] = matchString("foo").map(_ => 1)
  val barRouter: Router[Symbol] = matchString("bar").map(_ => Symbol("x"))
  val bazRouter: Router[Double] = matchString("baz").map(_ => 0.0)
  val quxRouter: Router[Char] = matchString("qux").map(_ => 'z')

  val fooBarRouter: Router[Int :+: Symbol :+: CNil] = fooRouter :+: barRouter
  val bazQuxRouter: Router[Double :+: Char :+: CNil] = bazRouter :+: quxRouter

  type All = Int :+: Symbol :+: Double :+: Char :+: CNil

  val allRouter: Router[All] = fooBarRouter :+: bazQuxRouter

  assert(allRouter("foo") == Some(Coproduct[All](1)))
  assert(allRouter("bar") == Some(Coproduct[All](Symbol("x"))))
  assert(allRouter("baz") == Some(Coproduct[All](0.0)))
  assert(allRouter("qux") == Some(Coproduct[All]('z')))
  assert(allRouter("unknown") == None)
}
