structure MySusp =
struct
  datatype 'a Comp = Comp of (unit -> 'a) | Value of 'a
  type 'a Susp = 'a Comp ref

  fun delay f = ref (Comp f)

  fun force s =
    case !s of Value a => a
             | Comp f => let val a = f() in (s := Value a; a) end
end

