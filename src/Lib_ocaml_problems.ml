module Lib_ocaml_problems : sig
  val last : 'a list -> 'a option
  val penultimate : 'a list -> 'a option
  val kth_element : 'a list -> int -> 'a option
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

    let rec kth_element lst k =
      match (k, lst) with
        | (_, []) -> None
        | (0, head::_) -> Some head
        | (_, _::rest) -> kth_element rest (k - 1)
end