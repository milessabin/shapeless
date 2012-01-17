package shapeless

object NMap {
  import Remover._
  import HList._
  
  implicit def nmap[K <: HList, V](map : Map[K, V]) = new NestedMap[K, V](map)
  
  class NestedMap[K <: HList, V](map : Map[K, V]) {
    /**
      * Return the subset of `map` that contains keys that contain `element`. 
      *
      * Proof of concept, not implemented with efficiency in mind.
      */
    def apply[E](element : E)(implicit remover : Remover[K, E]) : Map[remover.Out, V] = map.map { 
      case (key, value) => (remover(key) match {
        case (e, remainder) if (e == element) => Some(remainder)
        case _ => None
      }, value)
    }.collect {
      case (Some(key), value) => (key, value)
    }
  }
}