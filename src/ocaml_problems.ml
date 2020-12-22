let rec last lst = 
  match lst with
    | [] -> None
    | (_::rest) -> last rest