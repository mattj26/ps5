(*
                                Helper functions for
                                CS51 Problem Set 5
*)
module WT = Webtypes;;
exception Crawler_Error of string;;
exception DictSet_Empty_List;;

let add_key_pairs (words : string list)
                  (url : WT.LinkSet.elt)
                  (dict : WT.LinkIndex.dict)
                : WT.LinkIndex.dict =
  List.fold_right
    (fun w d->
      let links = WT.LinkIndex.lookup dict w in
      match links with
      | None -> WT.LinkIndex.insert d w (WT.LinkSet.singleton url)
      | Some s ->
          let newS = WT.LinkSet.insert s url in
          WT.LinkIndex.insert d w newS) words dict;;

let unwrap (op : 'a option) (erText : string) =
  match op with
  | None -> raise (Crawler_Error erText)
  | Some x -> x;;

let rec remove_list (ele : 'a) (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | hd::tl ->
      if ele = hd
      then tl
      else hd :: remove_list ele tl;;

let remove_return_list (lst : 'a list) : ('a, 'a list) =
  match lst with
  | [] -> raise (DictSet_Empty_List)
  | hd::tl -> (hd, tl);;






