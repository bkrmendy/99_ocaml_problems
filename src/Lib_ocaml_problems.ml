module Lib_ocaml_problems : sig
  val last : 'a list -> 'a option
  val penultimate : 'a list -> 'a option
  val kth_element : 'a list -> int -> 'a option
  val length : 'a list -> int
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

    let length lst =
      let rec length1 lst len =
        match lst with
          | [] -> len
          | (_::rest) -> length1 rest (len + 1) in
      length1 lst 0

end