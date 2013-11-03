structure Stream =
struct
  open SMLofNJ.Susp

  datatype 'a streamCell = Cons of 'a * 'a stream | Nil
  withtype 'a stream = 'a streamCell susp
  exception SHd and STl and Nth

  fun sHd s = case force s of Nil => raise SHd
                            | Cons (a, _) => a

  fun sTl s = case force s of Nil => raise STl
                            | Cons (_, t) => t

  fun append s1 s2 =
    case force s1 of Nil => s2
                   | Cons (a, s1') => delay (fn () => Cons (a, append s1' s2))

  fun constStream a = delay (fn () => Cons (a, constStream a))
  fun mkStream f =
  let
    fun go n = delay (fn () => Cons (f n, go (n + 1)))
  in
    go 0
  end

  fun from x = mkStream (fn n => n + x)
  fun sMap f s =
    case force s of Nil => delay (fn () => Nil)
                  | Cons (a, s') => delay (fn () => Cons (f a, sMap f s'))

  fun sDrop 0 s = s
    | sDrop n s =
    case force s of Nil => raise Nth
                  | Cons (_, s') => sDrop (n-1) s'

  fun sTake 0 _ = nil
    | sTake n s = case force s of Nil => nil
                                | Cons (a, s') => a :: sTake (n-1) s'

  fun zip s1 s2 =
    case force s1 of
         Nil => delay (fn () => Nil)
       | Cons (a, s1') =>
         case force s2 of
            Nil => delay (fn () => Nil)
          | Cons (b, s2') => delay (fn () => Cons ((a,b), zip s1' s2'))

  fun unzip s =
    case force s of
        Nil => (delay (fn () => Nil), delay (fn () => Nil))
      | Cons ((a,b),s') =>
          let
            val (sa, sb) = unzip s'
          in
            (delay (fn () => Cons (a, sa)), delay (fn () => Cons (b, sb)))
          end

  fun nth 0 s = sHd s
    | nth n s = nth (n-1) (sTl s)

  fun splice s1 s2 = let
    fun diagonal k 0 = delay (fn () =>
      Cons ( (nth (k-1) s1, nth 0 s2), delay (fn () => Nil)))
      | diagonal k i = delay (fn () =>
          Cons ( (nth (k-1-i) s1, nth i s2), diagonal k (i-1)))
    fun line k = append (diagonal k (k - 1))
      (delay (fn () => force (line (k + 1))))
  in
    delay (fn () => Cons ((nth 0 s1, nth 0 s2), line 2))
  end

  fun filter f s =
    case force s of
        Nil => delay (fn () => Nil)
      | Cons (a, s') => case f a of
                           NONE => filter f s'
                         | SOME b => delay (fn () => Cons (b, filter f s'))

  fun find f s = filter (fn a => if f a then SOME a else NONE) s
  fun remove f s = find (fn a => not (f a)) s
  fun flatten s =
    case force s of
        Nil => delay (fn () => Nil)
      | Cons (s, s') => append s (flatten s')

  fun drop 0 s = s
    | drop n s = case force s of
                     Nil => delay (fn () => Nil)
                   | Cons (_, s') => drop (n - 1) s'

  fun fromList [] = delay (fn () => Nil)
    | fromList (x::xs) =
      delay (fn () => Cons(x, fromList xs))
end


