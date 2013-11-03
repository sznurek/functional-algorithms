use "Stream.sml";;

structure InfDepthTree =
struct
  open SMLofNJ.Susp
  datatype 'a tree = Leaf | Node of 'a * 'a tree susp Stream.stream

  fun constTree x =
  let
    fun r () = Node (x, delay s)
    and s () = Stream.Cons (delay r, delay s)
  in
    r ()
  end

  fun tMap f Leaf = Leaf
    | tMap f (Node (x, s)) =
      Node (f x, Stream.sMap (fn t => delay (fn () => tMap f (force t))) s)

  fun subTree nil t = t
    | subTree (n::ns) Leaf = Leaf
    | subTree (n::ns) (Node (x, s)) =
      subTree ns (force (Stream.nth n s))
end
