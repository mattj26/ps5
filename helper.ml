(*
                                Helper functions for
                                CS51 Problem Set 5
*)
module WT = Webtypes;;
module D = Dict;;
exception Crawler_Error of string;;

let rec split_lists (lst1 : string list)
                (lst2 : string list)
                (len : int)
              : string list * string list =
  match lst1 with
  | [] -> raise (Crawler_Error "Splitting array error")
  | hd::tl ->
      if List.length lst1 > 140000
      then split_lists tl (hd::lst2) (len - 1)
      else (lst1, lst2)

let add_key_pairs (words : string list)
                  (url : WT.LinkSet.elt)
                  (dict : WT.LinkIndex.dict)
                : WT.LinkIndex.dict =
  let inner_add (lst : string list) (d2 : WT.LinkIndex.dict) : WT.LinkIndex.dict =
    let res =
    List.fold_right
      (fun w d->
        let links = WT.LinkIndex.lookup dict w in
        match links with
        | None -> WT.LinkIndex.insert d w (WT.LinkSet.singleton url)
        | Some s ->
            let newS = WT.LinkSet.insert s url in
            WT.LinkIndex.insert d w newS) lst d2 in
      res in
  if List.length words < 140000
  then inner_add words dict
  else
    let (x, y) = split_lists words [] (List.length words) in
    inner_add x dict |> inner_add y;;

let unwrap (op : 'a option) (erText : string) =
  match op with
  | None -> raise (Crawler_Error erText)
  | Some x -> x;;

let print_toFile (s : string) =
  let filename = "testout.txt" in
  let file = open_out filename in
  output_string file s;;

let size_of_set (s : WT.LinkSet.set) =
  WT.LinkSet.fold (fun x _ -> x + 1) 0 s;;





