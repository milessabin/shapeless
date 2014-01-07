/*
 * Copyright (c) 2011-13 Miles Sabin 
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

import sbt._

object Boilerplate {

  import scala.StringContext._

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }

  
  def gen(dir : File) = { 
    val tupler = dir / "shapeless" / "tupler.scala"
    IO.write(tupler, GenTuplerInstances.body)
    
    val fntoproduct = dir / "shapeless" / "fntoproduct.scala"
    IO.write(fntoproduct, GenFnToProductInstances.body)
    
    val fnfromproduct = dir / "shapeless" / "fnfromproduct.scala"
    IO.write(fnfromproduct, GenFnFromProductInstances.body)
    
    val caseinst = dir / "shapeless" / "caseinst.scala"
    IO.write(caseinst, GenCaseInst.body)

    val polyapply = dir / "shapeless" / "polyapply.scala"
    IO.write(polyapply, GenPolyApply.body)

    val polyinst = dir / "shapeless" / "polyinst.scala"
    IO.write(polyinst, GenPolyInst.body)

    val cases = dir / "shapeless" / "cases.scala"
    IO.write(cases, GenCases.body)

    val polyntraits = dir / "shapeless" / "polyntraits.scala"
    IO.write(polyntraits, GenPolyNTraits.body)        

    val nats = dir / "shapeless" / "nats.scala"
    IO.write(nats, GenNats.body)
    
    val tupletypeables = dir / "shapeless" / "tupletypeables.scala"
    IO.write(tupletypeables, GenTupleTypeableInstances.body)

    val sizedbuilder = dir / "shapeless" / "sizedbuilder.scala"
    IO.write(sizedbuilder, GenSizedBuilder.body)
    
    val hmapbuilder = dir / "shapeless" / "hmapbuilder.scala"
    IO.write(hmapbuilder, GenHMapBuilder.body)
    
    Seq(
      tupler, fntoproduct, fnfromproduct, caseinst, polyapply,
      polyinst, cases, polyntraits, nats, tupletypeables, sizedbuilder,
      hmapbuilder
    )
  }  

  val header = """
    |/*
    | * Copyright (c) 2011-13 Miles Sabin 
    | *
    | * Licensed under the Apache License, Version 2.0 (the "License");
    | * you may not use this file except in compliance with the License.
    | * You may obtain a copy of the License at
    | *
    | *     http://www.apache.org/licenses/LICENSE-2.0
    | *
    | * Unless required by applicable law or agreed to in writing, software
    | * distributed under the License is distributed on an "AS IS" BASIS,
    | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    | * See the License for the specific language governing permissions and
    | * limitations under the License.
    | */
    |
    |package shapeless
  """.stripMargin

  class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity) map (n => (n+'A').toChar)
    val synVals      = (0 until arity) map (n => (n+'a').toChar)
    val synTypedVals = (synVals zip synTypes) map { case (v,t) => v + ":" + t}

    val `A..N`       = synTypes.mkString(", ")
    val `A..N,Res`   = (synTypes :+ "Res") mkString ", "
    val `a..n`       = synVals.mkString(", ")
    val `A::N`       = (synTypes :+ "HNil") mkString "::"
    val `a::n`       = (synVals :+ "HNil") mkString "::"
    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`     = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`   = synTypedVals mkString ", "    
  }

  trait Template {
    def content(tv: TemplateVals): String
    def range = 1 to 22
    def body: String = {
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {_ filter (_ startsWith "-") map (_.tail) }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
    }
  }

  object GenTuplerInstances extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      block"""
        |package ops
        |
        |import hlist.Tupler
        |
        |trait TuplerInstances {
        |  type Aux[L <: HList, Out0] = Tupler[L] { type Out = Out0 }
        -
        -  implicit def hlistTupler${arity}
        -    [${`A..N`}]
        -  : Aux[
        -    ${`A::N`},
        -    ${`(A..N)`}
        -  ] =
        -    new Tupler[${`A::N`}] {
        -      type Out = ${`(A..N)`}
        -      def apply(l : ${`A::N`}): Out = l match { case ${`a::n`} => ${`(a..n)`} }
        -    }        
        |}
      """
    }      
  }
  
  object GenFnToProductInstances extends Template {
    override val range = 0 to 22

    def content(tv: TemplateVals) = {
      import tv._
      val fnType = s"(${`A..N`}) => Res"
      val hlistFnType = s"(${`A::N`}) => Res"
      val fnBody = if (arity == 0) "fn()" else s"l match { case ${`a::n`} => fn(${`a..n`}) }" 
      
      block"""
        |package ops
        |
        |import function.FnToProduct
        |
        |trait FnToProductInstances {
        |  type Aux[F, Out0] = FnToProduct[F] { type Out = Out0 }
        -
        -  implicit def fnToProduct${arity}
        -    [${`A..N,Res`}]
        -  : Aux[
        -    (${fnType}),
        -    ${hlistFnType}
        -  ] =
        -    new FnToProduct[${fnType}] {
        -      type Out = ${hlistFnType}
        -      def apply(fn: ${fnType}): Out
        -        = (l : ${`A::N`})
        -          => ${fnBody}
        -    }
        |}
      """
    }
  }
  
  object GenFnFromProductInstances extends Template {
    override val range = 0 to 22

    def content(tv: TemplateVals) = {
      import tv._
      val fnType = s"(${`A..N`}) => Res"
      val hlistFnType = s"(${`A::N`}) => Res"

      block"""
        |package ops
        |
        |import function.FnFromProduct
        |
        |trait FnFromProductInstances {
        |  type Aux[F, Out0] = FnFromProduct[F] { type Out = Out0 }
        |
        -  implicit def fnFromProduct${arity}
        -    [${`A..N,Res`}]
        -  : Aux[
        -    ${hlistFnType},
        -    ${fnType}
        -  ] = 
        -    new FnFromProduct[${hlistFnType}] {
        -      type Out = ${fnType}
        -      def apply(hf : ${hlistFnType}): Out
        -        = (${`a:A..n:N`})
        -          => hf(${`a::n`})
        -    }
        -
        |}
      """
    }
    
  }
  
  object GenCaseInst extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      block"""
        |
        |trait CaseInst {
        |  import poly._
        |
        -  implicit def inst${arity}
        -    [Fn <: Poly, ${`A..N`}, Res]
        -    (cse : Case[Fn, ${`A::N`}] { type Result = Res }) 
        -  : (${`A..N`}) => Res =
        -    (${`a:A..n:N`})
        -      => cse.value(${`a::n`})
        -
        |}
      """
    }
  }

  object GenPolyApply extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      block"""
        |
        |trait PolyApply {
        |  import poly._
        -  def apply
        -    [${`A..N`}]
        -    (${`a:A..n:N`})
        -    (implicit cse : Case[this.type, ${`A::N`}])
        -  : cse.Result =
        -    cse(${`a::n`})
        -
        |}
      """
    }


  }

  object GenPolyInst extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      block"""
        |
        |trait PolyInst {
        |
        -  implicit def inst${arity}
        -    [${`A..N`}]
        -    (fn : Poly)(implicit cse : fn.ProductCase[${`A::N`}])
        -  : (${`A..N`}) => cse.Result =
        -    (${`a:A..n:N`})
        -      => cse(${`a::n`})
        -
        |}
      """
    }
  }

  object GenCases extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      block"""
        |
        |trait Cases {
        |  import poly._
        |
        -  type Case${arity}[Fn, ${`A..N`}]
        -    = Case[Fn, ${`A::N`}]
        -
        -  object Case${arity} {
        -    type Aux[Fn, ${`A..N`}, Result0]
        -      = Case[Fn, ${`A::N`}] { type Result = Result0 }
        -
        -    def apply
        -      [Fn, ${`A..N`}, Result0]
        -      (fn: (${`A..N`}) => Result0)
        -    : Aux[Fn, ${`A..N`}, Result0] =
        -      new Case[Fn, ${`A::N`}] {
        -        type Result = Result0
        -        val value = (l: ${`A::N`})
        -          => l match {
        -            case ${`a::n`} =>
        -              fn(${`a..n`})
        -          }
        -      }
        -  }
        -
        |}
      """
    }
    
  }

  object GenPolyNTraits extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      val fnBody = if (arity == 0) "fn()" else s"l match { case ${`a::n`} => fn(${`a..n`}) }" 

      block"""
        |
        -
        -trait Poly${arity} extends Poly { outer =>
        -  type Case[${`A..N`}]
        -    = poly.Case[this.type, ${`A::N`}]
        -
        -  object Case {
        -    type Aux[${`A..N`}, Result0]
        -      = poly.Case[outer.type, ${`A::N`}] { type Result = Result0 }
        -  }
        -
        -  class CaseBuilder[${`A..N`}] {
        -    def apply[Res]
        -      (fn: (${`A..N`}) => Res) = new Case[${`A..N`}] {
        -      type Result = Res
        -      val value = (l: ${`A::N`})
        -        => ${fnBody}
        -    }
        -  }
        -  
        -  def at[${`A..N`}]
        -    = new CaseBuilder[${`A..N`}]
        -}
        |
      """
    }    
  }
  
  object GenNats extends Template {
    def content(tv: TemplateVals) = {
      val n = tv.arity
      block"""
        |
        |trait Nats {
        -
        -  type _${n} = Succ[_${n-1}]
        -  val _${n}: _${n} = new _${n}
        |}
      """
    }
  }
  
  object GenTupleTypeableInstances extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      val implicitArgs = (synTypes map(a => s"cast${a}:Typeable[${a}]")) mkString ", "
      val enumerators = synTypes.zipWithIndex map { case (a,idx) => s"_ <- p._${idx+1}.cast[${a}]" } mkString "; "

      block"""
        |
        |trait TupleTypeableInstances {
        |  import syntax.typeable._
        |
        -  implicit def tuple${arity}Typeable
        -    [${`A..N`}]
        -    (implicit ${implicitArgs})
        -  = new Typeable[${`(A..N)`}] {
        -    def cast(t : Any) : Option[${`(A..N)`}] = {
        -      if(t == null) Some(t.asInstanceOf[${`(A..N)`}])
        -      else if(t.isInstanceOf[${`(_.._)`}]) {
        -        val p = t.asInstanceOf[${`(_.._)`}]
        -        for(${enumerators})
        -        yield t.asInstanceOf[${`(A..N)`}]
        -      } else None
        -    }
        -  }
        -
        |}
      """
    }        
  }
  
  object GenSizedBuilder extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      val `a:T..n:T` = synVals map (_ + ":T") mkString ", "

      block"""
        |
        |class SizedBuilder[CC[_]] {
        |  import scala.collection.generic.CanBuildFrom
        |  import nat._
        |  import Sized.wrap
        |
        -  def apply[T](${`a:T..n:T`})
        -    (implicit cbf : CanBuildFrom[Nothing, T, CC[T]]) = 
        -    wrap[CC[T], _${arity}]((cbf() += (${`a..n`})).result)
        -
        |}
      """
    }
  }
  
  object GenHMapBuilder extends Template {
    def content(tv: TemplateVals) = {
      import tv._
      val typeArgs  = (0 until arity) map (n => s"K${n}, V${n}") mkString ", "
      val args      = (0 until arity) map (n => s"e${n}: (K${n}, V${n})") mkString ", "
      val witnesses = (0 until arity) map (n => s"ev${n}: R[K${n}, V${n}]") mkString ", "
      val mapArgs   = (0 until arity) map (n => "e"+n) mkString ", "

      block"""
        |
        |class HMapBuilder[R[_, _]] {
        -
        -  def apply
        -    [${typeArgs}]
        -    (${args})
        -    (implicit ${witnesses})
        -    = new HMap[R](Map(${mapArgs}))
        |}
      """
    }
  }

}
