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
    if count > n || WT.LinkSet.is_empty fRem
    then
      dict
    else
      let Some (link, setRem) = WT.LinkSet.choose fRem in
      let Some {WT.url = url; links; words} = CS.get_page link in
      inner_crawl (WT.LinkSet.union setRem links) (WT.LinkSet.insert vis url)
      (Helper.add_key_pairs words url dict) (count + 1) in
    inner_crawl frontier visited d 0;;





let crawler (num_pages_to_search : int) (initial_link : WT.link) =
  crawl num_pages_to_search
    (WT.LinkSet.singleton initial_link)
    WT.LinkSet.empty
    WT.LinkIndex.empty ;;
