use "Heap.sml";

functor BinomialHeap (Elem : ELEM) =
struct
  datatype Tree = Tree of int * Elem.T * Tree list
  type Heap = Tree list

  val empty = nil
  fun isEmpty nil = true
    | isEmpty _   = false

  fun link (t1 as Tree (r, x, c1), t2 as Tree (_, y, c2)) =
    if Elem.leq (x, y)
      then Tree (r + 1, x, t2 :: c1)
      else Tree (r + 1, y, t1 :: c2)

  fun rank (Tree (r, _, _)) = r

  fun insTree (t, []) = [t]
    | insTree (t, ts as t' :: tss) =
      if rank t < rank t' then t :: ts
      else if rank t > rank t' then t' :: insTree (t, tss)
      else insTree (link (t, t'), tss)

  fun insert (h, x) = insTree (Tree(0, x, []), h)

  fun merge ([], h) = h
    | merge (h, []) = h
    | merge (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
      if rank t1 < rank t2 then t1 :: merge (ts1', ts2)
      else if rank t1 > rank t2 then t2 :: merge (ts1, ts2')
      else insTree (link (t1, t2), merge (ts1', ts2'))

  fun root (Tree (_, x, _)) = x

  fun removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
      let val (t', ts') = removeMinTree ts
      in
        if Elem.leq (root t, root t')
          then (t, ts)
          else (t', t :: ts')
      end

  fun findMin h =
    let val (t, _) = removeMinTree h
    in root t end

  fun deleteMin h =
    let val (Tree (r, x, ts'), ts) = removeMinTree h
    in merge (rev ts', ts) end
end

structure BinomialIntHeap = BinomialHeap(IntElem)
