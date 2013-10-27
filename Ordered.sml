signature ELEM =
sig
  type T
  val leq : T * T -> bool
  val eq : T * T -> bool
end
