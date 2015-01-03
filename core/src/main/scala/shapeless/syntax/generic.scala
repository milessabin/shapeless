package shapeless
package syntax

final class GenericOps[T, R](gen: Generic.Aux[T, R]) {
  import ops.generic._

  def withLabels[L <: HList](implicit
    withLabels0: WithLabels[R, L]
  ): LabelledGeneric.Aux[T, withLabels0.Out] =
    new LabelledGeneric[T] {
      type Repr = withLabels0.Out
      def to(t: T) = withLabels0.to(gen.to(t))
      def from(r: withLabels0.Out) = gen.from(withLabels0.from(r))
    }
}
