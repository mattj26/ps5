(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

The crawler, which builds a dictionary from words to sets of
links.
 *)

(* Rename modules for convenience *)
module WT = Webtypes ;;
module CS = Crawler_services ;;

(* Only look at pagerank if you plan on implementing it! *)
module PR = Pagerank ;;

(*----------------------------------------------------------------------
  Section 1: CRAWLER
 *)

(* TODO: Replace the implementation of the crawl function (currently
   just a stub returning the empty dictionary) with a proper index of
   crawled pages. Build an index as follows:

   Remove a link from the frontier (the set of links that have yet to
   be visited), visit this link, add its outgoing links to the
   frontier, and update the index so that all words on this page are
   mapped to linksets containing this url.

   Keep crawling until we've reached the maximum number of links (n) or
   the frontier is empty.
 *)
exception Crawler_Error of string;;

let unwrap (op : 'a option) (erText : string) =
  match op with
  | None -> raise (Crawler_Error erText)
  | Some x -> x;;

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
            WT.LinkIndex.insert d w newS) words dict

let crawl (n : int)
          (frontier : WT.LinkSet.set)
          (visited : WT.LinkSet.set)
          (d : WT.LinkIndex.dict)
        : WT.LinkIndex.dict =
  let rec inner_crawl (fRem : WT.LinkSet.set)
                  (vis : WT.LinkSet.set)
                  (dict : WT.LinkIndex.dict)
                  (count: int)
                : WT.LinkIndex.dict =
    if count >= n || WT.LinkSet.is_empty fRem
    then
      dict
    else
      let link, setRem =
        unwrap (WT.LinkSet.choose fRem)
        "Link selection returns empty" in
      if WT.LinkSet.member vis link
      then inner_crawl setRem vis dict count
      else
        let search = CS.get_page link in
        match search with
        | None -> inner_crawl setRem (WT.LinkSet.insert vis link) dict count
        | Some {WT.url = url; links; words} ->
            inner_crawl (WT.LinkSet.union setRem links) (WT.LinkSet.insert vis url)
            (add_key_pairs words url dict) (count + 1) in
      inner_crawl frontier visited d 0;;


let crawler (num_pages_to_search : int) (initial_link : WT.link) =
  Gc.set { (Gc.get()) with Gc.stack_limit = 64 * 1024 * 1024};
  crawl num_pages_to_search
    (WT.LinkSet.singleton initial_link)
    WT.LinkSet.empty WT.LinkIndex.empty;;

