/*
 * Copyright (c) 2013-15 Miles Sabin
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

import syntax.typeable._

import reflect.ClassTag

import org.junit.Test
import org.junit.Assert._

import java.nio.ByteBuffer

package rpcService {

  trait Iface {
    def add(a: Int, b: Int): Int
    def concat(s1: String, s2: String): String
  }

  object Impl extends Iface {
    def add(a: Int, b: Int): Int = a + b
    def concat(s1: String, s2: String): String = s1 ++ s2
  }

}

package rpcFramework {

  trait Endpoint {
    def call(rpcRequest: RpcRequest): Either[String, RpcResponse]
  }
  object Endpoint {
    object ops {
      implicit class ToEndpointOp[T](val t: T) extends AnyVal {
        def toEndpoint[U >: T](implicit tet: ToEndpoint[U]): Endpoint = {
          tet.apply(t)
        }
      }
      implicit class ToClientOp(val ep: Endpoint) extends AnyVal {
        def toClient[T](implicit tc: ToClient[T]): T = tc(ep)
      }
    }
  }

  trait ToClient[T] {
    def apply(endpoint: Endpoint): T
  }
  object ToClient {
    implicit def instance[T, Repr <: HList, F <: HList](implicit
      iface: FlatInterface.Aux[T, Repr],
      sf: SynthesizeFuncs[Repr]
    ): ToClient[T] = {
      new ToClient[T] {
        def apply(ep: Endpoint): T = {
          val funcs = sf.synthesizeFuncs(ep).funcs
          iface.from(funcs)
        }
      }
    }
  }

  trait SynthesizeFunc[Func] {
    def synthesizeFunc(af: Args => Either[String, RpcResponse]): Func
  }
  object SynthesizeFunc {
    implicit def synthesizeFunc[
      A <: HList,
      R
    ](implicit
      ac: ArgsCodec[A],
      rc: ResponseCodec[R]
    ): SynthesizeFunc[A => R] = {
      new SynthesizeFunc[A => R] {
        def synthesizeFunc(af: Args => Either[String, RpcResponse]): A => R = { a =>
          af(ac.toWire(a)).right.flatMap(rc.fromWire) match {
            case Right(res) => res
            case Left(err) => throw new Exception(s"RPC Error: $err")
          }
        }
      }
    }
  }

  trait Funcs[F <: HList] {
    def funcs: F
    def index: Int
  }

  trait SynthesizeFuncs[F <: HList] {
    def synthesizeFuncs(ep: Endpoint): Funcs[F]
  }
  object SynthesizeFuncs {
    implicit val synthesizeHNil: SynthesizeFuncs[HNil] = new SynthesizeFuncs[HNil] {
      def synthesizeFuncs(ep: Endpoint): Funcs[HNil] = new Funcs[HNil] {
        def funcs = HNil
        def index = -1
      }
    }
    implicit def synthesizeHCons[
      H,
      T <: HList
    ](implicit
      sh: SynthesizeFunc[H],
      st: SynthesizeFuncs[T]
    ): SynthesizeFuncs[H :: T] = {
      new SynthesizeFuncs[H :: T] {
        def synthesizeFuncs(ep: Endpoint): Funcs[H :: T] = {
          new Funcs[H :: T] {
            val t = st.synthesizeFuncs(ep)
            def index = t.index + 1
            def funcs = sh.synthesizeFunc(args => ep.call(RpcRequest(index, args))) :: t.funcs
          }
        }
      }
    }
  }

  trait ToHandler[T] {
    def toHandler(t: T): Args => Either[String, RpcResponse]
  }
  object ToHandler {
    implicit def fromFunction[A <: HList, R](implicit
      ac: ArgsCodec[A],
      rc: ResponseCodec[R]
    ): ToHandler[A => R] = new ToHandler[A => R] {
      def toHandler(af: A => R) = { args =>
        ac.fromWire(args).right.map(a => rc.toWire(af(a)))
      }
    }
  }

  trait ToHandlers[T] {
    def toHandlers(t: T): List[Args => Either[String, RpcResponse]]
  }
  object ToHandlers {
    implicit val hnilToHandlers: ToHandlers[HNil] = new ToHandlers[HNil] {
      def toHandlers(hnil: HNil): List[Args => Either[String, RpcResponse]] = Nil
    }
    implicit def hconsToHandlers[H, T <: HList](implicit
      hToHandler: ToHandler[H],
      tToHandlers: ToHandlers[T]
    ): ToHandlers[H :: T] = new ToHandlers[H :: T] {
      def toHandlers(hcons: H :: T): List[Args => Either[String, RpcResponse]] = {
        hToHandler.toHandler(hcons.head) :: tToHandlers.toHandlers(hcons.tail)
      }
    }
  }

  trait ToEndpoint[T] {
    def apply(t: T): Endpoint
  }
  object ToEndpoint {
    def apply[T](implicit tet: ToEndpoint[T]) = tet

    implicit def instance[T, Repr <: HList, F <: HList](implicit
      iface: FlatInterface.Aux[T, Repr],
      th: ToHandlers[Repr]
    ): ToEndpoint[T] = {
      new ToEndpoint[T] {
        def apply(t: T) = new Endpoint {
          val repr = iface.to(t)
          val handlers = th.toHandlers(repr).toVector.reverse
          def call(request: RpcRequest) = {
            if (request.op <= handlers.size) handlers(request.op)(request.args)
            else Left("No handler for method")
          }
        }
      }
    }
  }

  trait ArgsCodec[T] {
    def toWire(t: T): Args
    def fromWire(args: Args): Either[String, T]
  }
  object ArgsCodec {
    implicit val hnilArgsCodec: ArgsCodec[HNil] = new ArgsCodec[HNil] {
      def toWire(hn: HNil): Args = Args(hn)
      def fromWire(args: Args): Either[String, HNil] =
        args.raw match {
          case HNil => Right(HNil)
          case other => Left("Invalid args")
        }
    }
    implicit def hconsArgsCodec[H: Typeable, T <: HList: ArgsCodec: Typeable]: ArgsCodec[H :: T] =
      new ArgsCodec[H :: T] {
        def toWire(hl: H :: T): Args = Args(hl)
        def fromWire(args: Args): Either[String, H :: T] = {
          args.raw.cast[H :: T] match {
            case Some(h :: t) => {
              implicitly[ArgsCodec[T]]
                .fromWire(Args(t))
                .right.map(t => h :: t)
            }
            case None => Left("Invalid args")
          }
        }
      }
  }

  trait ResponseCodec[T] {
    def toWire(t: T): RpcResponse
    def fromWire(response: RpcResponse): Either[String, T]
  }
  object ResponseCodec {
    implicit def rpcResponseCodec[T: Typeable]: ResponseCodec[T] = {
      new ResponseCodec[T] {
        def toWire(t: T): RpcResponse = RpcResponse(t)
        def fromWire(response: RpcResponse): Either[String, T] = {
          response.raw.cast[T] match {
            case Some(t) => Right(t)
            case None => Left("Invalid response")
          }
        }
      }
    }
  }

  case class RpcRequest(op: Int, args: Args)
  case class Args(raw: Any)
  case class RpcResponse(raw: Any)

}

package generationFramework {

  trait Gen[T] {
    def generate: T
  }
  object Gen {
    def apply[T](implicit instance: Gen[T]): Gen[T] = instance
    implicit val genInt: Gen[Int] = new Gen[Int] {
      def generate: Int = util.Random.nextInt
    }
    implicit val genString: Gen[String] = new Gen[String] {
      def generate: String = util.Random.nextInt.toString
    }
    implicit def genFunction[A, R](implicit
      genR: Gen[R]
    ): Gen[A => R] = new Gen[A => R] {
      def generate: (A => R) = {
        val res: R = genR.generate
        _ => res
      }
    }
    implicit val genHNil: Gen[HNil] = new Gen[HNil] {
      def generate: HNil = HNil
    }
    implicit def genHCons[H, T <: HList](implicit
      gh: Gen[H],
      gt: Gen[T]
    ): Gen[H :: T] = new Gen[H :: T] {
      def generate: H :: T = gh.generate :: gt.generate
    }
    implicit def genInstance[T, Repr](implicit
      iface: FlatInterface.Aux[T, Repr],
      genRepr: Gen[Repr]
    ): Gen[T] = new Gen[T] {
      def generate = iface.from(genRepr.generate)
    }
  }

}

package tracingFramework {
  import labelled._
  import collection.mutable.ListBuffer
  trait Show[T] {
    def show(t: T): String
  }
  object Show {
    def apply[T](implicit instance: Show[T]): Show[T] = instance
    implicit val showString: Show[String] = new Show[String] {
      def show(str: String) = "\""++str++"\""
    }
    implicit val showInt: Show[Int] = new Show[Int] {
      def show(i: Int) = i.toString
    }
  }

  trait ShowArg[T] {
    def showArg(t: T): String
  }
  object ShowArg {
    implicit def instance[K <: Symbol, T](implicit
      kw: Witness.Aux[K],
      st: Show[T],
      tt: Typeable[T]
    ): ShowArg[FieldType[K, T]] = new ShowArg[FieldType[K, T]] {
      def showArg(t: FieldType[K, T]): String = {
        s"${kw.value.name} = ${st.show(t)}: ${tt.describe}"
      }
    }
  }

  trait ShowArgList[T <: HList] {
    def showArgList(t: T): List[String]
  }
  object ShowArgList {
    implicit val showHNil: ShowArgList[HNil] = new ShowArgList[HNil] {
      def showArgList(hnil: HNil): List[String] = Nil
    }
    implicit def showHCons[H, T <: HList](implicit
      sh: ShowArg[H],
      st: ShowArgList[T]
    ): ShowArgList[H :: T] = new ShowArgList[H :: T] {
      def showArgList(hc: H :: T): List[String] = sh.showArg(hc.head) :: st.showArgList(hc.tail)
    }
  }

  trait ShowArgLists[T <: HList] {
    def showArgLists(t: T): List[List[String]]
  }
  object ShowArgLists {
    implicit val showHNil: ShowArgLists[HNil] = new ShowArgLists[HNil] {
      def showArgLists(hnil: HNil): List[List[String]] = Nil
    }
    implicit def showHCons[H <: HList, T <: HList](implicit
      sh: ShowArgList[H],
      st: ShowArgLists[T]
    ): ShowArgLists[H :: T] = new ShowArgLists[H :: T] {
      def showArgLists(hc: H :: T): List[List[String]] = sh.showArgList(hc.head) :: st.showArgLists(hc.tail)
    }
  }

  trait Tracer[T] {
    def traced(t: T, buff: ListBuffer[String]): T
  }
  object Tracer {
    def apply[T](implicit instance: Tracer[T]): Tracer[T] = instance

    implicit val traceHNil: Tracer[HNil] = new Tracer[HNil] {
      def traced(hnil: HNil, buff: ListBuffer[String]): HNil = hnil
    }

    implicit def traceHCons[H, T <: HList](implicit
      ht: Tracer[H],
      tt: Tracer[T]
    ): Tracer[H :: T] = new Tracer[H :: T] {
      def traced(hc: H :: T, buff: ListBuffer[String]): H :: T = {
        ht.traced(hc.head, buff) :: tt.traced(hc.tail, buff)
      }
    }

    implicit def traceFunction[K <: Symbol, A <: HList, R](implicit
      k: Witness.Aux[K],
      sa: ShowArgLists[A],
      sr: Show[R],
      tr: Typeable[R]
    ): Tracer[FieldType[K, A => R]] = new Tracer[FieldType[K, A => R]] {
      def traced(f: FieldType[K, A => R], buff: ListBuffer[String]): FieldType[K, A => R] = field[K] { a: A =>
        val args = sa.showArgLists(a).map(_.mkString("(", ", ", ")")).mkString
        val callString = k.value.name++args
        val res = f(a)
        val resString = sr.show(res) ++ ": " ++ tr.describe
        buff += s"$callString => $resString"
        res
      }
    }
    implicit def traceInterface[T, Repr](implicit
      iface: LabelledNestedInterface.Aux[T, Repr],
      tr: Tracer[Repr]
    ): Tracer[T] = new Tracer[T] {
      def traced(t: T, buff: ListBuffer[String]): T = iface.from(tr.traced(iface.to(t), buff))
    }
  }
}


class InterfaceTests {

  @Test
  def testSynthesizeRpc {

    import rpcFramework._
    import rpcService._
    import Endpoint.ops._

    val ep: Endpoint = Impl.toEndpoint[Iface]

    val client = ep.toClient[Iface]

    assert(client.add(3,4) == 7)
    assert(client.concat("x", "y") == "xy")
  }

  @Test
  def testSynthesizeInstance {
    import generationFramework._
    import rpcService._
    val instance = Gen[Iface].generate
    val res: Int = instance.add(3, 4)
  }

  @Test
  def testTracer {
    import tracingFramework._
    import rpcService._
    import collection.mutable.ListBuffer
    val log = ListBuffer[String]()
    val traced = Tracer[Iface].traced(Impl, log)
    traced.add(4,5)
    traced.concat("a", "b")
    val expected = List(
      """add(a = 4: Int, b = 5: Int) => 9: Int""",
      """concat(s1 = "a": String, s2 = "b": String) => "ab": String"""
    )
    assert(log.toList == expected)
  }
}
