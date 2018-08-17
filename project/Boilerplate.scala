/*
 * Copyright (c) 2011-18 Miles Sabin
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

/**
 * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
 * and would be tedious to craft by hand
 *
 * @author Miles Sabin
 * @author Kevin Wright
 */
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

  
  val templates: Seq[Template] = List(
    GenTuplerInstances,
    GenFnToProductInstances,
    GenFnFromProductInstances,
    GenCaseInst,
    GenPolyApply,
    GenPolyInst,
    GenCases,
    GenPolyNTraits,
    GenPolyNBuilders,
    GenNats,
    GenTupleTypeableInstances,
    GenSizedBuilder,
    GenHMapBuilder,
    GenUnpackInstances
  )

  /** Returns a seq of the generated files.  As a side-effect, it actually generates them... */
  def gen(dir : File) = for(t <- templates) yield {
    val tgtFile = dir / "shapeless" / t.filename
    IO.write(tgtFile, t.body)
    tgtFile
  }  

  val header = """
    |/*
    | * Copyright (c) 2011-18 Miles Sabin
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
    val `_.._`       = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`     = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`   = synTypedVals mkString ", "    
  }

  trait Template {
    def filename: String
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


  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body

      - The contents of the `header` val is output first

      - Then the first block of lines beginning with '|'

      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity

      - Then the last block of lines prefixed with '|'

    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
  */
  
  object GenTuplerInstances extends Template {
    val filename = "tupler.scala"
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
    val filename = "fntoproduct.scala"

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
    val filename = "fnfromproduct.scala"

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
    val filename = "caseinst.scala"

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
    val filename = "polyapply.scala"

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
    val filename = "polyinst.scala"

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
    val filename = "cases.scala"

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
    val filename = "polyntraits.scala"

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
        -
        -object Poly${arity} extends PolyNBuilders.Poly${arity}Builder[HNil] {
        - val functions = HNil
        -}
        |
      """
    }
  }

  object GenPolyNBuilders extends Template {
    val filename = "polynbuilders.scala"

    def content(tv: TemplateVals) = {
      import tv._

      block"""
        |
        |
        |/**
        |  * Provides elegant syntax for creating polys from functions
        |  *
        |  * @author Aristotelis Dossas
        |  */
        |object PolyNBuilders {
        -
        - trait Poly${arity}Builder[HL <: HList] { self =>
        -
        -   val functions: HL
        -   class AtAux[${`A..N`}] {
        -     def apply[Out](λ: (${`A..N`}) => Out) = {
        -       new Poly${arity}Builder[((${`A..N`}) => Out) :: HL] {
        -         val functions = λ :: self.functions
        -       }
        -     }
        -   }
        -   def at[${`A..N`}] = new AtAux[${`A..N`}]
        -
        -   def build = new Poly${arity} {
        -     val functions = self.functions
        -
        -     implicit def allCases[${`A..N`}, Out](implicit tL: Function${arity}TypeAt[${`A..N`}, Out, HL]) = {
        -       val func: (${`A..N`}) => Out = tL(functions)
        -       at(func)
        -     }
        -   }
        - }
        -
        - /* For internal use of Poly${arity}Builder */
        - trait Function${arity}TypeAt[${`A..N`}, Out, HL <: HList] {
        -   def apply(l: HL): (${`A..N`}) => Out
        - }
        -
        - object Function${arity}TypeAt {
        -   implicit def at0[${`A..N`}, Out, Tail <: HList] = new Function${arity}TypeAt[${`A..N`}, Out, ((${`A..N`}) => Out)::Tail] {
        -     def apply(l: ((${`A..N`}) => Out)::Tail): (${`A..N`}) => Out = {
        -       l.head
        -     }
        -   }
        -
        -   implicit def atOther[${`A..N`}, Out, Tail <: HList, Head](implicit tprev: Function${arity}TypeAt[${`A..N`}, Out, Tail]) = new Function${arity}TypeAt[${`A..N`}, Out, Head::Tail] {
        -     def apply(l: Head::Tail): (${`A..N`}) => Out = {
        -       tprev(l.tail)
        -     }
        -   }
        - }
        |}
      """
    }    
  }
  
  object GenNats extends Template {
    val filename = "nats.scala"

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
    val filename = "tupletypeables.scala"

    def content(tv: TemplateVals) = {
      import tv._
      val implicitArgs = (synTypes map(a => s"cast${a}:Typeable[${a}]")) mkString ", "
      val enumerators = synTypes.zipWithIndex map { case (a,idx) => s"_ <- p._${idx+1}.cast[${a}]" } mkString "; "
      val castVals = (synTypes map(a => s"$${cast${a}.describe}")) mkString ", "

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
        -      if(t == null) None
        -      else if(t.isInstanceOf[${`(_.._)`}]) {
        -        val p = t.asInstanceOf[${`(_.._)`}]
        -        for(${enumerators})
        -        yield t.asInstanceOf[${`(A..N)`}]
        -      } else None
        -    }
        -    override def describe = s"($castVals)"
        -  }
        -
        |}
      """
    }        
  }
  
  object GenSizedBuilder extends Template {
    val filename = "sizedbuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._
      val `a:T..n:T` = synVals map (_ + ":T") mkString ", "

      block"""
        |
        |class SizedBuilder[CC[_]] {
        |  import scala.collection._
        |  import nat._
        |  import Sized.wrap
        |
        -  def apply[T](${`a:T..n:T`})
        -    (implicit dis: DefaultToIndexedSeq[CC], factory : Factory[T, CC[T]], ev : AdditiveCollection[CC[T]]) =
        -    wrap[CC[T], _${arity}]((factory.newBuilder ++= Seq(${`a..n`})).result)
        -
        |}
      """
    }
  }
  
  object GenHMapBuilder extends Template {
    val filename = "hmapbuilder.scala"

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
 
  object GenUnpackInstances extends Template {
    val filename = "unpack.scala"
    def content(tv: TemplateVals) = {
      import tv._
      val typeblock = "FF[" + `A..N` + "], FF, " + `A..N`
      val hktypeblock = "FF[" + `_.._` + "], " + `A..N`
      val traitname = s"Unpack${arity}"
      block"""
        |
        -
        -/**
        - * Type class witnessing that type `PP` is equal to `FF[${`A..N`}]` for some higher kinded type `FF[${`_.._`}]` and type(s) `${`A..N`}`.
        - * 
        - * @author Miles Sabin
        - */
        -trait $traitname[-PP, $hktypeblock]
        -
        -object $traitname {
        -  implicit def unpack[$hktypeblock]: $traitname[$typeblock] = new $traitname[$typeblock] {}
        -}
        |
      """
    }      
  }
}
