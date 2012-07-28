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

object ReflectionUtils {
  import scala.reflect.base.{ MirrorOf, TreeCreator, TypeCreator, Universe }
  import scala.reflect.runtime.currentMirror
  import scala.reflect.runtime.universe._
  import scala.tools.reflect.Eval

  val booleanTypeId = Select(Ident(newTermName("scala")), newTypeName("Boolean"))
  val intTypeId = Select(Ident(newTermName("scala")), newTypeName("Int"))
  val doubleTypeId = Select(Ident(newTermName("scala")), newTypeName("Double"))
  val stringTypeId = Select(Select(Ident(newTermName("java")), newTermName("lang")), newTypeName("String"))

  def anyToTypeId(a : Any) = a match {
    case _ : Boolean => booleanTypeId
    case _ : Int => intTypeId
    case _ : Double => doubleTypeId
    case _ : String => stringTypeId
  }

  def mkExpr[T : AbsTypeTag](mirror: Mirror)(tree : Tree) : mirror.universe.Expr[T] =
    mirror.universe.Expr[T](mirror, new TreeCreator {
      def apply[U <: Universe with Singleton](m : MirrorOf[U]) : U#Tree =
        if (m eq mirror) tree.asInstanceOf[U#Tree]
        else throw new IllegalArgumentException(s"Expr defined in $mirror cannot be migrated to other mirrors.")
    })
}

object StagedTypeClassExample extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.currentMirror
  import scala.tools.reflect.Eval
  import ReflectionUtils._

  trait TupleConsumer[A, B] {
    def apply(t : (A, B)) : String
  }

  object TupleConsumer {
    implicit def intString = new TupleConsumer[Int, String] {
      def apply(t : (Int, String)) = t._1+t._2
    }
    implicit def booleanDouble = new TupleConsumer[Boolean, Double] {
      def apply(t : (Boolean, Double)) = (if(t._1) "+" else "-")+t._2
    }
  }

  def consumeTuple[A, B](t : (A, B))(implicit tc : TupleConsumer[A, B]) : String = tc(t)

  def stagedConsumeTuple(rawTuple : (Any, Any)) : String = {
    val tpe1 = anyToTypeId(rawTuple._1)
    val tpe2 = anyToTypeId(rawTuple._2)

    val tupleTree =
      TypeApply(
        Select(
          Ident(newTermName("rawTuple")),
          newTermName("asInstanceOf")),
        List(
          AppliedTypeTree(
            Select(
              Ident(newTermName("scala")),
              newTypeName("Tuple2")),
            List(tpe1, tpe2))))

    val consumeTree =
      Apply(
        Select(
          Select(
            Select(
              Ident(newTermName("shapeless")),
              newTermName("examples")),
            newTermName("StagedTypeClassExample")),
          newTermName("consumeTuple")),
        List(tupleTree))

    val consumeExpr = mkExpr[String](currentMirror)(consumeTree)

    val fnExpr = reify { (rawTuple : (Any, Any)) => consumeExpr.splice }

    val fn = fnExpr.eval
    fn(rawTuple)
  }

  val t1 : (Any, Any) = (23, "foo") // Specific element types erased
  val t2 : (Any, Any) = (true, 2.0) // Specific element types erased

  // Type class instances selected on static type at runtime!

  val c1 = stagedConsumeTuple(t1) // Uses intString instance
  assert(c1 == "23foo")
  println(c1)

  val c2 = stagedConsumeTuple(t2) // Uses booleanDouble instance
  assert(c2 == "+2.0")
  println(c2)
}
