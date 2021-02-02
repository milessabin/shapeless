/*
 * Copyright (c) 2020 Miles Sabin
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

package shapeless3.data

import scala.compiletime._

trait Monoidal {
  type to[_] <: Tuple
  type from[_ <: Tuple]

  type compose[f[_], g[_]] = [t] =>> g[f[t]]

  type length[m] = Monoidal.length[to[m]]
  type reverse[m] = from[Monoidal.reverse[to[m]]]
  type flatten[mm] = from[Monoidal.flatten[Monoidal.map[to[mm]][to]]]
  type map[m] = [f[_]] =>> from[Monoidal.map[to[m]][f]]
  type flatMap[m] = [f[_]] =>> from[Monoidal.flatten[Monoidal.map[to[m]][compose[f, to]]]]
  type select[m, n <: Int] = from[Monoidal.select[to[m], n]]
}

trait Cartesian extends Monoidal {
  type reversePrepend[m, n] = from[Monoidal.reversePrepend[to[m], to[n]]]
  type concat[m, n] = from[Monoidal.concat[to[m], to[n]]]
  type zipWith[m, n] = [f[_, _]] =>> from[Monoidal.zipWith[to[m], to[n]][f]]
  type at[m, n <: Int] = Monoidal.at[to[m], n]
}

trait Cocartesian extends Monoidal {
  type reverseExtend[m, n] = from[Monoidal.reversePrepend[to[m], to[n]]]
  type extend[m, n] = from[Monoidal.concat[to[m], to[n]]]
}

object Monoidal {
  type length[m <: Tuple] = m match {
    case EmptyTuple => 0
    case hd *: tl => S[length[tl]]
  }

  type reverse[m <: Tuple] = reverseAcc[m, EmptyTuple]
  type reverseAcc[m <: Tuple, acc <: Tuple] <: Tuple = m match {
    case hd *: tl => reverseAcc[tl, hd *: acc]
    case EmptyTuple => acc
  }

  type map[m <: Tuple] = [f[_]] =>> map0[m, f]
  type map0[m <: Tuple, f[_]] <: Tuple = m match {
    case EmptyTuple => EmptyTuple
    case hd *: tl => f[hd] *: map0[tl, f]
  }

  type flatten[mm <: Tuple] = flattenAcc[mm, EmptyTuple]
  type flattenAcc[mm <: Tuple, acc <: Tuple] <: Tuple = mm match {
    case hd *: tl => flattenAcc[tl, reversePrepend[hd, acc]]
    case EmptyTuple => reverse[acc]
  }

  type reversePrepend[m <: Tuple, n <: Tuple] <: Tuple = m match {
    case hd *: tl => reversePrepend[tl, hd *: n]
    case EmptyTuple => n
  }

  type concat[m <: Tuple, n <: Tuple] = reversePrepend[reverse[m], n]

  type zipWith[m <: Tuple, n <: Tuple] = [f[_, _]] =>> zipWithAcc[m, n, EmptyTuple, f]
  type zipWithAcc[m <: Tuple, n <: Tuple, acc <: Tuple, f[_, _]] <: Tuple = (m, n) match {
    case (hm *: tm, hn *: tn) => zipWithAcc[tm, tn, f[hm, hn] *: acc, f]
    case (EmptyTuple, EmptyTuple) => reverse[acc]
  }

  type at[m <: Tuple, n <: Int] = (m, n) match {
    case (hd *: _, 0) => hd
    case (_ *: tl, S[p]) => at[tl, p]
  }

  type select[m <: Tuple, n <: Int] = selectAcc[m, n, EmptyTuple]
  type selectAcc[m <: Tuple, n <: Int, acc <: Tuple] <: Tuple = (m, n) match {
    case (hd *: tl, 0) => hd *: reversePrepend[acc, tl]
    case (hd *: tl, S[p]) => selectAcc[tl, p, hd *: acc]
  }
}

trait UnboundedMonoidal[T0[_, _], U0] extends Monoidal{
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case U0 => EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => T0[hd, from[tl]]
    case EmptyTuple => U0
  }
}

trait BoundedMonoidal[B, T0[_, _ <: B] <: B, U0 <: B] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case U0 => EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => T0[hd, from[tl]]
    case EmptyTuple => U0
  }
}

trait UnitlessMonoidal[T0[_, _]] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case _ => t *: EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => from0[hd, tl]
  }
  type from0[p, t <: Tuple] = t match {
    case hd *: tl => T0[p, from0[hd, tl]]
    case EmptyTuple => p
  }
}

trait DirectMonoidal[T0[_ <: Tuple]] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[l] => l
  }
  type from[t <: Tuple] = T0[t]
}

object tuples extends Monoidal with Cartesian {
  type to[t] = t&Tuple
  type from[t <: Tuple] = t
}

object pairs extends UnboundedMonoidal[Tuple2, Unit] with Cartesian

object eithers extends Cocartesian {
  // Nothing needs special handling ... perhaps we should drop this
  // encoding of coproducts altogether?
  //
  // The equivalent of CNil would be Nothing and no match type applied
  // to Nothing will reduce, so bare CNil is more or less impossible to
  // express. That being so, the unitless version is probably a better
  // choice.
  class Wrap[T]

  type to[t] <: Tuple = Wrap[t] match {
    case Wrap[Either[hd, tl]] => hd *: to[tl]
    case Wrap[Nothing] => EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => Either[hd, from[tl]]
    case EmptyTuple => Nothing
  }
}

object pairs2 extends UnitlessMonoidal[Tuple2] with Cartesian

object eithers2 extends UnitlessMonoidal[Either] with Cocartesian
