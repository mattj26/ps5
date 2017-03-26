(*
                                Helper functions for
                                CS51 Problem Set 5
*)
  module WT = Webtypes;;

let add_key_pairs (words : string list)
                  (url : WT.LinkSet.elt)
                  (dict : WT.LinkIndex.dict)
                : WT.LinkIndex.dict =
  let url_as_set = WT.LinkSet.singleton url in
  List.fold_right (fun w d-> WT.LinkIndex.insert d w url_as_set) words dict;;



