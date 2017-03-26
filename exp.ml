
let printHash (obj : 'a) : unit =
  print_endline ("Hash: " ^ string_of_int (Hashtbl.hash obj));;
