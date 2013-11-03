structure BinInfDepthTree =
struct
  open SMLofNJ.Susp
  datatype 'a tree = Leaf | Node of 'a tree susp * 'a * 'a tree susp

  fun constTree x =
  let
    fun s () = r ()
    and r () = Node (delay s, x, delay s)
  in
    r ()
  end

  fun tMap f Leaf = Leaf
    | tMap f (Node (l, x, r)) =
      let
        val left = fn () => tMap f (force l)
        val right = fn () => tMap f (force r)
      in
        Node (delay left, f x, delay right)
      end

  datatype dir = L | R

  fun subtree nil t = t
    | subtree (L::ds) Leaf = Leaf
    | subtree (L::ds) (Node (l, _, _)) = subtree ds (force l)
    | subtree (R::ds) Leaf = Leaf
    | subtree (R::ds) (Node (_, _, r)) = subtree ds (force r)

  fun mirror Leaf = Leaf
    | mirror (Node (l, x, r)) =
      let
        val left = fn () => mirror (force r)
        val right = fn () => mirror (force l)
      in
        Node (delay left, x, delay right)
      end

  val naturalTree =
    let
      fun buildFrom n =
        Node (delay (fn () => buildFrom (2*n)), n,
              delay (fn () => buildFrom (2*n+1)))
    in
      buildFrom 1
    end
end
