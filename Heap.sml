use "Ordered.sml";
exception exc

signature HEAP =
sig
  structure Elem : ELEM
  type Heap
  val empty : Heap
  val isEmpty : Heap -> bool
  val insert : Heap * Elem.T -> Heap
  val findMin : Heap -> Elem.T
  val deleteMin : Heap -> Heap
  val merge : Heap * Heap -> Heap
end

structure IntElem : ELEM =
struct
  type T = int
  fun leq (x, y) = x <= y
  fun eq (x, y) = x == y
end

