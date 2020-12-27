open List

type 'a rle = 
    | Single of 'a
    | Multiple of int * 'a;;

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
  val pack : 'a list -> 'a list list
  val encode : 'a list -> (int * 'a) list
  val encode_adt : 'a list -> 'a rle list
  val decode : 'a rle list -> 'a list
  val duplicate : 'a list -> 'a list
  val replicate : 'a list -> int -> 'a list
  val drop_nth : 'a list -> int -> 'a list
  val split : 'a list -> int -> 'a list * 'a list
  val slice : 'a list -> int -> int -> 'a list
  val rotate : 'a list -> int -> 'a list
  val remove_at : 'a list -> int -> 'a list
  val insert_at : 'a -> int -> 'a list -> 'a list
  val range : int -> int -> int list
  val extract : int -> 'a list -> 'a list list
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
    
    let rec pack2 lst acc =
      match lst with
        | [] -> acc
        | (head::rest) ->
          match acc with
            | [] -> pack2 rest [[head]]
            | ([]::arest) -> pack2 rest ([head]::arest)
            | ((re::rrest)::arest) ->
              if re = head then
                pack2 rest ((head::re::rrest)::arest)
              else
                pack2 lst ([]::acc)

    let pack lst = pack2 lst [] |> List.rev

    let rec encode2 lst acc =
      match lst with
        | [] -> acc
        | (head::rest) ->
          match acc with
            | [] -> encode2 rest [(1, head)]
            | ((n, value)::arest) ->
              if value = head then
                encode2 rest ((n + 1, value)::arest)
              else
                encode2 rest ((1, head)::acc)

    let encode lst = encode2 lst [] |> List.rev

    let rec encode_adt2 lst acc =
      match lst with
        | [] -> acc
        | (head::rest) ->
          match acc with
            | [] -> encode_adt2 rest [(Single head)]
            | ((Single value)::arest) ->
              if value = head then
                encode_adt2 rest ((Multiple (2, head))::arest)
              else
                encode_adt2 rest ((Single head)::acc)
            | ((Multiple (n, value))::arest) ->
              if value = head then
                encode_adt2 rest ((Multiple (n + 1, head))::arest)
              else
                encode_adt2 rest ((Single head)::acc)

    let encode_adt lst = encode_adt2 lst [] |> List.rev

    let rec repeat value times =
      match times with
        | 0 -> []
        | n -> value::(repeat value (n - 1))

    let rec decode2 lst acc =
      match lst with
        | [] -> acc
        | ((Single value)::rest) -> decode2 rest (value::acc)
        | ((Multiple (n, value))::rest) -> decode2 rest (List.concat [(repeat value n); acc])

    let decode lst = decode2 lst [] |> List.rev

    let rec duplicate lst =
      match lst with
        | [] -> []
        | (head::rest) -> (head::head::(duplicate rest))
    
    let rec replicate lst n =
      if n < 0 then []
      else
        match lst with
          | [] -> []
          | (head::rest) -> List.concat [(repeat head n); (replicate rest n)]

    let drop_nth lst n = 
      let rec aux lst acc c =
        match (lst, c) with
          | ([], _) -> acc
          | (_::rest, 0) -> (aux rest acc n)
          | (head::rest, cx) -> (aux rest (head::acc) (cx - 1)) in
      aux lst [] n |> List.rev

    let split lst at = 
      let rec aux lst2 acc at =
        match at with
          | 0 -> (List.rev acc, lst2)
          | n ->
            match lst2 with
              | [] -> (List.rev acc, [])
              | (head::rest) -> aux rest (head::acc) (n-1) in
      aux lst [] at

    let rec take lst n =
      match (lst, n) with
        | ([], _) -> []
        | (_, 0) -> []
        | (head::rest, _) -> head::(take rest (n - 1))

    let rec drop lst n =
      match (lst, n) with
        | ([], _) -> []
        | (_, 0) -> lst
        | (_::rest, c) -> drop rest (c - 1)

    let rec slice lst from until =
      match (lst, from) with
        | ([], _) -> []
        | (_, 0) -> take lst (until - from + 1)
        | (_::rest, _) -> slice rest (from - 1) (until - 1)

    let rotate lst by =
      let n = if by > 0 then
        by
      else
        (length lst) + by in
        List.concat [drop lst n; take lst n]

    let rec remove_at lst idx =
      match (lst, idx) with
        | ([], _) -> []
        | (_::rest, 0) -> rest
        | (head::rest, _) -> head::(remove_at rest (idx - 1))
    
    let rec insert_at elem idx lst =
      match (lst, idx) with
        | (_, 0) -> elem::lst
        | ([], _) -> []
        | (head::rest, _) -> head::(insert_at elem (idx - 1) rest)

    let rec range from until = 
      if from = until then [until]
      else if from > until then from::(range (from - 1) until)
      else from::(range (from + 1) until)

    let rec extract n lst =
      if n < 1 then [[]]
      else match lst with
        | [] -> []
        | head::rest ->
          let with_h = List.map (fun l -> head :: l) (extract (n-1) rest) in
          let without_h = extract n rest in
            with_h @ without_h;;
end