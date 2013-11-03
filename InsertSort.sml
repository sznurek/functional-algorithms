use "Stream.sml";;
open Stream

fun insert x s =
  case force s of
      Nil => fromList [x]
    | Cons (y, ys) =>
        if x < y
          then delay (fn () => Cons (x, s))
          else delay (fn () => Cons (y, insert x ys))

fun insort s =
  case force s of
     Nil => fromList []
   | Cons (x, xs) => insert x (insort xs)
