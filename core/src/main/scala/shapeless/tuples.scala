package shapeless

object Tuple {
  import ops.tuple._

  case class FillBuilder[N]() {
    def apply[A](elem: A)(implicit fill: Fill[N, A]) : fill.Out = fill(elem)
  }

  /**
   * Produces a tuple of length `N` filled with `elem`.
   */
  def fill(n: Nat) = FillBuilder[n.N]()
  
  /**
   * Produces a `N1`-length tuple made of `N2`-length tuples filled with `elem`.
   */
  def fill(n1: Nat, n2: Nat) = FillBuilder[(n1.N, n2.N)]()
  
}
