exception NotFound

signature FiniteMap =
sig
  type Key
  type 'a Map
  val empty : 'a Map
  val bind : Key * 'a * 'a Map -> 'a Map
  val lookup : Key * 'a Map -> 'a
end

