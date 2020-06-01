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

import Monoidal._

trait Monoidal {
  type to[_]
  type from[_]

  type compose[f[_], g[_]] = [t] =>> g[f[t]]

  type length[m] = Monoidal.length[to[m]]
  type reverse[m] = from[Monoidal.reverse[to[m]]]
  type flatten[mm] = from[Monoidal.flatten[Monoidal.map[to[mm]][to]]]
  type map[m] = [f[_]] =>> from[Monoidal.map[to[m]][f]]
  type flatMap[m] = [f[_]] =>> from[Monoidal.flatten[Monoidal.map[to[m]][compose[f, to]]]]
  type select[m, n] = from[Monoidal.select[to[m], n]]
}

trait Cartesian extends Monoidal {
  type reversePrepend[m, n] = from[Monoidal.reversePrepend[to[m], to[n]]]
  type concat[m, n] = from[Monoidal.concat[to[m], to[n]]]
  type zipWith[m, n] = [f[_, _]] =>> from[Monoidal.zipWith[to[m], to[n]][f]]
  type at[m, n] = Monoidal.at[to[m], n]
}

trait Cocartesian extends Monoidal {
  type reverseExtend[m, n] = from[Monoidal.reversePrepend[to[m], to[n]]]
  type extend[m, n] = from[Monoidal.concat[to[m], to[n]]]
}

object Monoidal {
  class T[hd, tl]
  class U

  type length[m] = m match {
    case U => 0
    case T[hd, tl] => S[length[tl]]
  }

  type reverse[m] = reverseAcc[m, U]
  type reverseAcc[m, acc] = m match {
    case U => acc
    case T[hd, tl] => reverseAcc[tl, T[hd, acc]]
  }

  type map[m] = [f[_]] =>> map0[m, f]
  type map0[m, f[_]] = m match {
    case U => U
    case T[hd, tl] => T[f[hd], map0[tl, f]]
  }

  type flatten[mm] = flattenAcc[mm, U]
  type flattenAcc[mm, acc] = mm match {
    case U => reverse[acc]
    case T[hd, tl] => flattenAcc[tl, reversePrepend[hd, acc]]
  }

  type reversePrepend[m, n] = m match {
    case U => n
    case T[hd, tl] => reversePrepend[tl, T[hd, n]]
  }

  type concat[m, n] = reversePrepend[reverse[m], n]

  type zipWith[m, n] = [f[_, _]] =>> zipWithAcc[m, n, U, f]
  type zipWithAcc[m, n, acc, f[_, _]] = (m, n) match {
    case (U, U) => reverse[acc]
    case (T[hm, tm], T[hn, tn]) => zipWithAcc[tm, tn, T[f[hm, hn], acc], f]
  }

  type at[m, n] = (m, n) match {
    case (T[hd, _], 0) => hd
    case (T[_, tl], S[p]) => at[tl, p]
  }

  type select[m, n] = selectAcc[m, n, U]
  type selectAcc[m, n, acc] = (m, n) match {
    case (T[hd, tl], 0) => T[hd, reversePrepend[acc, tl]]
    case (T[hd, tl], S[p]) => selectAcc[tl, p, T[hd, acc]]
  }

  class Inv[t]
}

trait UnboundedMonoidal[T[_, _], U] extends Monoidal{
  type to[t] = UnboundedMonoidal.to[T, U][t]
  type from[t] = UnboundedMonoidal.from[T, U][t]
}

object UnboundedMonoidal {
  type to[T0[_, _], U0] = [t] =>> to0[T0, U0, t]
  type to0[T0[_, _], U0, t] = Inv[t] match {
    case Inv[U0] => U
    case Inv[T0[hd, tl]] => T[hd, to0[T0, U0, tl]]
  }

  type from[T0[_, _], U0] = [t] =>> from0[T0, U0, t]
  type from0[T0[_, _], U0, t] = t match {
    case U => U0
    case T[hd, tl] => T0[hd, from0[T0, U0, tl]]
  }
}

trait BoundedMonoidal[B, T[_, _ <: B] <: B, U <: B] extends Monoidal {
  type to[t] = BoundedMonoidal.to[B, T, U][t]
  type from[t] = BoundedMonoidal.from[B, T, U][t]
}

object BoundedMonoidal {
  type to[B, T0[_, _ <: B] <: B, U0 <: B] = [t] =>> to0[B, T0, U0, t]
  type to0[B, T0[_, _ <: B] <: B, U0 <: B, t] = Inv[t] match {
    case Inv[U0] => U
    case Inv[T0[hd, tl]] => T[hd, to0[B, T0, U0, tl]]
  }

  type from[B, T0[_, _ <: B], U0 <: B] = [t] =>> from0[B, T0, U0, t]
  type from0[B, T0[_, _ <: B], U0 <: B, t] = t match {
    case U => U0
    case T[hd, tl] => T0[hd, from0[B, T0, U0, tl]]
  }
}

trait UnitlessMonoidal[T[_, _]] extends Monoidal {
  type to[t] = UnitlessMonoidal.to[T][t]
  type from[t] = UnitlessMonoidal.from[T][t]
}

object UnitlessMonoidal {
  type to[T0[_, _]] = [t] =>> to0[T0, t]
  type to0[T0[_, _], t] = Inv[t] match {
    case Inv[T0[hd, tl]] => T[hd, to0[T0, tl]]
    case Inv[l] => T[l, U]
  }

  type from[T0[_, _]] = [t] =>> from0[T0, t]
  type from0[T0[_, _], t] = t match {
    case T[hd, tl] => from1[T0, hd, tl]
  }
  type from1[T0[_, _], p, t] = t match {
    case U => p
    case T[hd, tl] => T0[p, from1[T0, hd, tl]]
  }
}

trait DirectMonoidal[T[_]] extends Monoidal {
  type to[t] = DirectMonoidal.to[T][t]
  type from[t] = DirectMonoidal.from[T][t]
}

object DirectMonoidal {
  type to[T0[_]] = [t] =>> to0[T0, t]
  type to0[T0[_], t] = t match {
    case T0[l] => l
  }

  type from[T0[_]] = [t] =>> T0[t]
}

object pairs extends UnboundedMonoidal[Tuple2, Unit] with Cartesian

object tuples extends BoundedMonoidal[Tuple, *:, Unit] with Cartesian

object eithers extends UnboundedMonoidal[Either, Nothing] with Cocartesian

object pairs2 extends UnitlessMonoidal[Tuple2] with Cartesian

object eithers2 extends UnitlessMonoidal[Either] with Cocartesian
