use "Heap.sml";

functor ExplicitMin (H : HEAP) : HEAP =
struct
  structure Elem = H.Elem
  datatype Heap = E | NE of Elem.T * H.Heap
  val empty = E

  fun isEmpty E      = true
    | isEmpty (NE _) = false

  fun insert (E, v)         = NE (v, H.insert (H.empty, v))
    | insert (NE (m, h), v) =
      if Elem.leq (v, m)
        then NE (v, H.insert (h, v))
        else NE (m, H.insert (h, v))

  fun findMin E = raise exc
    | findMin (NE (m,_)) = m

  fun deleteMin E = raise exc
    | deleteMin (NE (m, h)) =
      let
        val h' = H.deleteMin h
      in
        if H.isEmpty h'
          then E
          else NE (H.findMin h', h')
      end

  fun merge (E, h) = h
    | merge (h, E) = h
    | merge ((NE (m1, h1)), (NE (m2, h2))) =
      let
        val h = H.merge (h1, h2)
      in
        if Elem.leq (m1, m2)
         then NE (m1, h)
         else NE (m2, h)
      end
end

