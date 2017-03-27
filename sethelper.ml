

exception DictSet_Empty_List;;

let rec remove_list (ele : 'a) (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | hd::tl ->
      if ele = hd
      then tl
      else hd :: remove_list ele tl;;

let remove_return_list (lst : 'a list) : ('a * 'a list) =
  match lst with
  | [] -> raise (DictSet_Empty_List)
  | hd::tl -> (hd, tl);;
