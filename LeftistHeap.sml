use "Heap.sml";

functor LeftistHeap (Elem : ELEM) =
struct
  structure Elem = Elem
  datatype Heap = E | T of int * Elem.T * Heap * Heap

  val empty = E
  fun isEmpty E = true
    | isEmpty _ = false

  fun rank E = 0
    | rank (T (r, _, _, _)) = r

  fun makeT (x, a, b) =
    if rank a >= rank b
      then T (rank b + 1, x, a, b)
      else T (rank a + 1, x, b, a)

  fun merge (E, h) = h
    | merge (h, E) = h
    | merge (h1 as T (_, x, l1, r1), h2 as T (_, y, l2, r2)) =
      if Elem.leq (x, y)
        then makeT (x, l1, merge (r1, h2))
        else makeT (y, l2, merge (r2, h1))

  fun insert (h, e) = merge (T (1, e, E, E), h)

  fun findMin E = raise exc
    | findMin (T (_, x, _, _)) = x

  fun deleteMin E = raise exc
    | deleteMin (T (_, _, l, r)) = merge (l, r)

  fun mergeHeapList nil = []
    | mergeHeapList [x] = [x]
    | mergeHeapList (x::y::xs) = merge (x,y) :: mergeHeapList xs

  fun mergeLoop [x] = x
    | mergeLoop xs  = mergeLoop (mergeHeapList xs)

  fun fromList xs =
    mergeLoop (map (fn x => insert (empty, x)) xs)
end

structure LeftistIntHeap = LeftistHeap(IntElem)
