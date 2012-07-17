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

trait TupleConsumer[A, B] {
  def apply(t : (A, B))
}
  
object TupleConsumer {
  implicit def intString = new TupleConsumer[Int, String] {
    def apply(t : (Int, String)) { println("Using [Int, String] instance:     "+t._1+t._2) }
  }
  implicit def booleanDouble = new TupleConsumer[Boolean, Double] {
    def apply(t : (Boolean, Double)) { println("Using [Boolean, Double] instance: "+(if(t._1) "+" else "-")+t._2) }
  }
}

object StagingExamples extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.currentMirror
  import scala.tools.reflect.Eval
  import ReflectionUtils._
  
  def consumeTuple[A, B](t : (A, B))(implicit tc : TupleConsumer[A, B]) = tc(t)

  val tuples : List[(Any, Any)] = List((23, "foo"), (true, 2.0))

  for(rawTuple @ (a, b) <- tuples) {
    val tpe1 = anyToTypeId(a)
    val tpe2 = anyToTypeId(b)
    
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
            newTermName("StagingExamples")),
          newTermName("consumeTuple")),
        List(tupleTree))
    
    val consumeExpr = mkExpr[Unit](currentMirror)(consumeTree)
        
    val fnExpr = reify { (rawTuple : (Any, Any)) => consumeExpr.splice }

    val fn = fnExpr.eval
    fn(rawTuple)
  }
}