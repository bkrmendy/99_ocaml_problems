open List

type 'a node =
    | One of 'a
    | Many of 'a node list;;

module Lib_ocaml_problems : sig
  val last : 'a list -> 'a option
  val penultimate : 'a list -> 'a option
  val kth_element : 'a list -> int -> 'a option
  val length : 'a list -> int
  val reverse : 'a list -> 'a list
  val is_palindrome : 'a list -> bool
  val flatten : 'a node list -> 'a list
  val compress : 'a list -> 'a list
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

    let reverse lst =
      let rec reverse1 lst1 acc = 
        match lst1 with
          | [] -> acc
          | (head::rest) -> reverse1 rest (head::acc) in
      reverse1 lst []

    let is_palindrome lst =
      let rev = reverse lst in
      lst = rev

    let rec flatten lst =
      match lst with
        | [] -> []
        | ((One elem)::rest) -> List.concat [[elem]; (flatten rest)]
        | ((Many elems)::rest) -> List.concat [(flatten elems); (flatten rest)]

    let rec compress lst =
      match lst with
        | [] -> []
        | (head::[]) -> [head]
        | (a::b::rest) -> if a = b then compress (a::rest) else (a::(compress (b::rest)))

end