/*
 * Copyright (c) 2011-14 Miles Sabin
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

object StagedTypeClassExample extends App {
  import scala.reflect.runtime.universe._
  import ReflectionUtils._

  trait TupleConsumer[A, B] {
    def apply(t: (A, B)): String
  }

  object TupleConsumer {
    implicit val intString = new TupleConsumer[Int, String] {
      def apply(t: (Int, String)) = t._1+t._2
    }

    implicit val booleanDouble = new TupleConsumer[Boolean, Double] {
      def apply(t: (Boolean, Double)) = (if(t._1) "+" else "-")+t._2
    }
  }

  def consumeTuple[A, B](t: (A, B))(implicit tc: TupleConsumer[A, B]): String = tc(t)

  def stagedConsumeTuple(rawTuple: (Any, Any)): String = {
    val tpt1 = mkTypeTree(rawTuple._1)
    val tpt2 = mkTypeTree(rawTuple._2)

    val fnTree =
      q"""
        (t: (Any, Any)) =>
          shapeless.examples.StagedTypeClassExample.consumeTuple(t.asInstanceOf[($tpt1, $tpt2)])
       """

       val fn = evalTree[((Any, Any)) => String](fnTree)

    fn(rawTuple)
  }

  val t1: (Any, Any) = (23, "foo") // Specific element types erased
  val t2: (Any, Any) = (true, 2.0) // Specific element types erased

  // Type class instances selected on static type at runtime!

  val c1 = stagedConsumeTuple(t1) // Uses intString instance
  assert(c1 == "23foo")

  val c2 = stagedConsumeTuple(t2) // Uses booleanDouble instance
  assert(c2 == "+2.0")
}

object ReflectionUtils {
  import scala.reflect.api.{ Mirror, TreeCreator, Universe }
  import scala.reflect.runtime.currentMirror
  import scala.reflect.runtime.universe._
  import scala.tools.reflect.Eval

  def mkTypeTree(a: Any): Tree = a match {
    case _: Byte    => tq"_root_.scala.Byte"
    case _: Char    => tq"_root_.scala.Char"
    case _: Short   => tq"_root_.scala.Short"
    case _: Int     => tq"_root_.scala.Int"
    case _: Long    => tq"_root_.scala.Long"
    case _: Float   => tq"_root_.scala.Float"
    case _: Double  => tq"_root_.scala.Double"
    case _: Boolean => tq"_root_.scala.Boolean"
    case _: Unit    => tq"_root_.scala.Unit"
    case other       => tq"${other.getClass.getName}"
  }

  def mkExpr[T: TypeTag](tree: Tree): Expr[T] =
    Expr[T](currentMirror, new TreeCreator {
      def apply[U <: Universe with Singleton](m: Mirror[U]): U#Tree =
        if (m eq currentMirror) tree.asInstanceOf[U#Tree]
        else throw new IllegalArgumentException(s"Expr defined in $currentMirror cannot be migrated to other mirrors.")
    })

  def evalTree[T: TypeTag](tree: Tree) = mkExpr[T](tree).eval
}
