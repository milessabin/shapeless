package shapeless

object Tuple {
  import ops.tuple._

  /**
   * Produces a tuple of length `N` filled with `elem`.
   */
  def fill[A](n: Nat)(elem: A)(implicit fill: Fill[n.N, A]) : fill.Out = fill(elem)
  
  /**
   * Produces a `N1`-length tuple made of `N2`-length tuples filled with `elem`.
   */
  def fill[A](n1: Nat, n2: Nat)(elem: A)(implicit fill: Fill[(n1.N, n2.N), A]) : fill.Out = fill(elem)
  
}
