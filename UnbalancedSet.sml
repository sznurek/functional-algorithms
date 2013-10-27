use "Set.sml";
use "Ordered.sml";

functor UnbalancedSet (Elem : ELEM) : SET =
struct
  type Elem = Elem.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E
  fun member (x, E) = false
    | member (x, T (l, y, r)) =
      if Elem.eq (x, y) then true
      else if Elem.leq (x, y)
        then member (x, l)
        else member (x, r)

  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (l, y, r)) =
      if Elem.eq (x, y) then s
      else if Elem.leq (x, y)
        then T (insert (x, l), y, r)
        else T (l, y, insert (x, r))
end
