(*
                                Helper functions for
                                CS51 Problem Set 5
*)
module WT = Webtypes;;
module D = Dict;;
exception Crawler_Error of string;;

let add_key_pairs (words : string list)
                  (url : WT.LinkSet.elt)
                  (dict : WT.LinkIndex.dict)
                : WT.LinkIndex.dict =
  Printf.printf "Adding key_pairs: %i\n" (List.length words);
  let res =
  List.fold_right
    (fun w d->
      let links = WT.LinkIndex.lookup dict w in
      match links with
      | None -> WT.LinkIndex.insert d w (WT.LinkSet.singleton url)
      | Some s ->
          let newS = WT.LinkSet.insert s url in
          WT.LinkIndex.insert d w newS) words dict in
    print_endline "Done adding key_pairs"; res;;

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





