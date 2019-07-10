/*
 * Copyright (c) 2019 Miles Sabin
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

import scala.annotation.tailrec
import scala.compiletime._
import scala.deriving._

abstract class ErasedInstances[FT] {
  def erasedMap(x: Any)(f: (Any, Any) => Any): Any
}

abstract class ErasedProductInstances[FT] extends ErasedInstances[FT] {
  def erasedConstruct(f: Any => Any): Any
  def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any])
  def erasedMap(x0: Any)(f: (Any, Any) => Any): Any
  def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any
  def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any
  def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any
}

final class ErasedProductInstances0[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
  def erasedConstruct(f: Any => Any): Any = mirror.fromProduct(None)
  def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = (a, Some(mirror.fromProduct(None)))
  def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = x0
  def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = x0
  def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = a
  def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = a
}

abstract class ErasedProductInstances1[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
  def mkI: Any
  lazy val i = mkI

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

  final def erasedConstruct(f: Any => Any): Any =
    mirror.fromProduct(Tuple1(f(i)))

  final def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
    val (acc0, e0) = f(a, i)
    e0 match {
      case Some(_) => (acc0, Some(mirror.fromProduct(e0)))
      case None => (acc0, None)
    }
  }

  final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any =
    mirror.fromProduct(Tuple1(f(i, toProduct(x0).productElement(0))))

  final def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any =
    mirror.fromProduct(Tuple1(f(i, toProduct(x0).productElement(0), toProduct(y0).productElement(0))))

  final def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    f(a, i, toProduct(x0).productElement(0)) match {
      case Complete(r) => r
      case Continue(acc) => acc
    }
  }

  final def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    f(a, i, toProduct(x0).productElement(0), toProduct(y0).productElement(0)) match {
      case Complete(r) => r
      case Continue(acc) => acc
    }
  }
}

abstract class ErasedProductInstancesN[FT](val mirror: Mirror.Product) extends ErasedProductInstances[FT] {
  import ErasedProductInstances.ArrayProduct

  def mkIs: Array[Any]
  lazy val is = mkIs

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

  final def erasedConstruct(f: Any => Any): Any = {
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  final def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
    val n = is.length
    val arr = new Array[Any](n)
    var acc = a
    var i = 0
    while(i < n) {
      val (acc0, e0) = f(acc, is(i))
      e0 match {
        case Some(e) =>
          acc = acc0
          arr(i) = e
        case None =>
          return (acc0, None)
      }
      i = i+1
    }
    (acc, Some(mirror.fromProduct(ArrayProduct(arr))))
  }

  final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
    val x = toProduct(x0)
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i), x.productElement(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  final def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = {
    val x = toProduct(x0)
    val y = toProduct(y0)
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i), x.productElement(i), y.productElement(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  final def erasedFoldLeft(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    val x = toProduct(x0)
    val n = x.productArity
    @tailrec
    def loop(i: Int, acc: Any): Any =
      if(i >= n) acc
      else
        f(acc, is(i), x.productElement(i)) match {
          case Complete(r) => r
          case Continue(acc) =>
            loop(i+1, acc)
        }

    loop(0, i)
  }

  final def erasedFoldLeft2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    val x = toProduct(x0)
    val y = toProduct(y0)
    val n = x.productArity
    @tailrec
    def loop(i: Int, acc: Any): Any =
      if(i >= n) acc
      else
        f(acc, is(i), x.productElement(i), y.productElement(i)) match {
          case Complete(r) => r
          case Continue(acc) =>
            loop(i+1, acc)
        }

    loop(0, i)
  }
}

object ErasedProductInstances {
  class ArrayProduct(val elems: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

  inline def summonOne[T] = inline erasedValue[T] match {
    case _: Tuple1[a] => summon[a]
  }

  inline def apply[FT, E <: Tuple](mirror: Mirror.Product) : ErasedProductInstances[FT] =
    inline erasedValue[Tuple.Size[E]] match {
      case 0 => new ErasedProductInstances0[FT](mirror)
      case 1 => new ErasedProductInstances1[FT](mirror) { def mkI = summonOne[E] }
      case _ => new ErasedProductInstancesN[FT](mirror) { def mkIs = summonAsArray[E] }
    }
}

abstract class ErasedCoproductInstances[FT](mirror: Mirror.Sum) extends ErasedInstances[FT] {
  def mkIs: Array[Any]
  lazy val is = mkIs

  def ordinal(x: Any): Any = is(mirror.ordinal(x.asInstanceOf))

  def erasedMap(x: Any)(f: (Any, Any) => Any): Any = {
    val i = ordinal(x)
    f(i, x)
  }

  def erasedProject(p: Int)(i: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) =
    f(i, is(p))

  def erasedFold(x: Any)(f: (Any, Any) => Any): Any = {
    val i = ordinal(x)
    f(i, x)
  }

  def erasedFold2(x: Any, y: Any)(a: => Any)(f: (Any, Any, Any) => Any): Any = {
    val i = mirror.ordinal(x.asInstanceOf)
    val j = mirror.ordinal(y.asInstanceOf)
    if(i == j) f(is(i), x, y)
    else a
  }
}

object ErasedCoproductInstances {
  inline def apply[FT, E <: Tuple](mirror: Mirror.Sum) : ErasedCoproductInstances[FT] =
    new ErasedCoproductInstances[FT](mirror) {
      def mkIs = summonAsArray[E]
    }
}
