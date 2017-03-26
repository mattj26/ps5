(*
                                Helper functions for
                                CS51 Problem Set 5
*)
  module WT = Webtypes;;

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






