module Lib_ocaml_problems : sig
  val last : 'a list -> 'a option
  val penultimate : 'a list -> 'a option
end = 
struct
    let rec last lst = 
      match lst with
        | [] -> None
        | (head::[]) -> Some head
        | (_::rest) -> last rest

    let rec penultimate lst =
      match lst with
        | [] -> None
        | (_::[]) -> None
        | (pen::_::[]) -> Some pen
        | (_::rest) -> penultimate rest
end