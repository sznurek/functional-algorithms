use "Ordered.sml";
use "FiniteMap.sml";
use "UnbalancedSet.sml";

functor UnbalancedMap (Elem : ELEM) : FiniteMap =
struct
  type Key = Elem.T
  datatype 'a Tree = E | T of 'a Tree * Elem.T * 'a * 'a Tree
  type 'a Map = 'a Tree

  val empty = E
  fun lookup (x, E) = raise NotFound
    | lookup (x, T (l, y, v, r)) =
      if Elem.eq (x, y) then v
      else if Elem.leq (x, y)
        then lookup (x, l)
        else lookup (x, r)

  fun bind (k, v, E) = T (E, k, v, E)
    | bind (k, v, s as T (l, k', v', r)) =
      if Elem.eq (k, k') then s
      else if Elem.leq (k, k')
        then T (bind (k, v, l), k', v', r)
        else T (l, k', v', bind (k, v, r))
end
