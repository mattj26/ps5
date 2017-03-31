(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

An interface and simple implementation of a set abstract datatype.
 *)

open Order ;;

(* Interface for set modules *)

module type SET =
  sig
    (* type of elements in the set *)
    type elt

    (* abstract type for the set *)
    type set

    val empty : set

    val is_empty : set -> bool

    val insert : set -> elt -> set

    val singleton : elt -> set

    val union : set -> set -> set

    val intersect : set -> set -> set

    (* remove an element from the set -- if the element isn't present,
      returns set unchanged *)
    val remove : set -> elt -> set

    (* returns true iff the element is in the set *)
    val member : set -> elt -> bool

    (* chooses some member from the set, removes it and returns that
       element plus the new set.  If the set is empty, returns
       None. *)
    val choose : set -> (elt * set) option

    (* fold a function across the elements of the set in some
       unspecified order, using the calling convention of fold_left,
       that is, if the set s contains s1, ..., sn, then
          fold f u s
       returns
          (f ... (f (f u s1) s2) ... sn)
     *)
    val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a

    (* functions to convert values of these types to a string
       representation; useful for debugging. *)
    val string_of_set : set -> string
    val string_of_elt : elt -> string

    (* runs the tests. See TESTING EXPLANATION *)
    val run_tests : unit -> unit
  end

(* COMPARABLE signature -- A module that provides for elements that
   can be compared as to ordering and converted to a string
   representation. Includes functinos for generating values for
   testing purposes.
 *)

module type COMPARABLE =
  sig
    type t
    val compare : t -> t -> ordering
    val string_of_t : t -> string

    (* The functions below are used for testing. See TESTING EXPLANATION *)

    (* Generate a value of type t. The same t is always returned *)
    val gen : unit -> t

    (* Generate a random value of type t. *)
    val gen_random : unit -> t

    (* Generate a t greater than the argument. *)
    val gen_gt : t -> t

    (* Generate a t less than the argument. *)
    val gen_lt : t -> t

    (* Generate a t between the two arguments. Return None if no such
       t exists. *)
    val gen_between : t -> t -> t option
  end

(* An example implementation of the COMPARABLE signature. Use this
   struct for testing. *)

module IntComparable : COMPARABLE =
  struct
    type t = int
    let compare x y =
      if x < y then Less
      else if x > y then Greater
      else Equal
    let string_of_t = string_of_int
    let gen () = 0
    let gen_random =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
    let gen_gt x = x + 1
    let gen_lt x = x - 1
    let gen_between x y =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end

(*----------------------------------------------------------------------
  Implementation 1: List-based implementation of sets, represented as
  sorted lists with no duplicates.
 *)

module ListSet (C: COMPARABLE) : (SET with type elt = C.t) =
  struct
    type elt = C.t
    type set = elt list

    (* INVARIANT: sorted, no duplicates *)
    let empty = []

    let is_empty xs =
      match xs with
      | [] -> true
      | _ -> false

    let singleton x = [x]

    let rec insert xs x =
      match xs with
      | [] -> [x]
      | y :: ys ->
          match C.compare x y with
          | Greater -> y :: (insert ys x)
          | Equal -> xs
          | Less -> x :: xs

    let union xs ys = List.fold_left insert xs ys

    let rec remove xs y =
      match xs with
      | [] -> []
      | x :: xs1 ->
          match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x :: (remove xs1 y)

    let rec intersect xs ys =
      match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh :: xt, yh :: yt ->
          match C.compare xh yh with
          | Equal -> xh :: (intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt

    let rec member xs x =
      match xs with
      | [] -> false
      | y :: ys ->
          match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false

    let choose xs =
      match xs with
      | [] -> None
      | x :: rest -> Some (x, rest)

    let fold = List.fold_left

    let string_of_elt = C.string_of_t

    let string_of_set (s: set) : string =
      let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
      "set([" ^ (List.fold_left f "" s) ^ "])"


    (* Tests for the ListSet functor -- These are just examples of
    tests, your tests should be a lot more thorough than these. *)

    (* adds a list of (key,value) pairs in left-to-right order *)
    let insert_list (d: set) (lst: elt list) : set =
      List.fold_left (fun r k -> insert r k) d lst

    let rec generate_random_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_random ()) :: (generate_random_list (size - 1))

    let test_insert () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      List.iter (fun k -> assert(member s1 k)) elts;
      ()

    let test_remove () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      let s2 = List.fold_right (fun k r -> remove r k) elts s1 in
      List.iter (fun k -> assert(not (member s2 k))) elts;
      ()

    let test_union () =
      ()

    let test_intersect () =
      ()

    let test_member () =
      ()

    let test_choose () =
      ()

    let test_fold () =
      ()

    let test_is_empty () =
      ()

    let test_singleton () =
      ()

    let run_tests () =
      test_insert () ;
      test_remove () ;
      test_union () ;
      test_intersect () ;
      test_member () ;
      test_choose () ;
      test_fold () ;
      test_is_empty () ;
      test_singleton () ;
      ()

  end

(*----------------------------------------------------------------------
  Implementation 2: Sets as dictionaries
 *)
(*
  TODO: Use the skeleton code for the DictSet module below and
  complete the implementation, making sure that it conforms to the
  appropriate signature.

  Add appropriate tests for the functor and make sure that your
  implementation passes the tests. Once you have the DictSet functor
  working, you can use it instead of the ListSet implementation by
  updating the definition of the Make functor below.
*)

exception Empty_Set of string;;

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =

   struct
    module D = Dict.Make(struct
        type key = int
        type value = C.t list

        let compare x y = if x < y then Less
          else if x >y then Greater else Equal
        let string_of_key = string_of_int
        let string_of_value v = List.fold_right
          (fun a b -> C.string_of_t a ^ " -> " ^ b) v "" |> String.trim
        let hash x = Hashtbl.hash x mod 100
        let gen_key () = hash 26
        let gen_key_random () =
          Random.self_init(); hash (Random.int 100)
        let gen_key_gt x = x + 1
        let gen_key_lt x = x - 1
        let gen_key_between x y =
          if x = y then None
          else
            let min, max = min x y, max x y in
            Random.self_init (); Some (Random.int (max - min - 1) + min + 1)
        let gen_value () = []
        let gen_pair () =
          Random.self_init ();
          let key = hash (Random.int 100) in
          (key, [])


      end)

    type elt = C.t
    type set = D.dict
    let empty = D.empty
    let is_empty x = (x = D.empty)
    let hash x = Hashtbl.hash x mod 100
    let singleton x = D.insert empty (hash x) [x]
    let hash_and_lookup s e =
      let hash = hash e in
      let look = D.lookup s hash in
      (hash, look)
    let insert s e =
      let hash = hash e in
      let look = D.lookup s hash in
      match look with
      | None -> D.insert s hash [e]
      | Some b -> if List.mem e b
                  then s
                  else D.insert s hash (e::b)
    let insert_key_list keys dict =
      List.fold_right (fun k d -> insert d k) keys dict
    let union s1 s2 =
      D.fold (fun d _ v -> insert_key_list v d) s1 s2
    let intersect s1 s2 =
      D.fold (fun d k v ->
                let look = D.lookup s2 k in
                match look with
                | None -> d
                | Some lst ->
                    List.fold_right
                      (fun ele res ->
                        if List.mem ele lst
                        then insert res ele
                        else res) v d) empty s1
    let rec remove_list (ele : 'a) (lst : 'a list) : 'a list =
      match lst with
      | [] -> []
      | hd::tl ->
        if ele = hd
        then tl
        else hd :: remove_list ele tl;;
    let remove s e =
      let hash = hash e in
      let look = D.lookup s hash in
      match look with
      | None -> s
      | Some lst ->
          if List.mem e lst
          then
            if List.length lst = 1
            then D.remove s hash
            else D.insert s hash (remove_list e lst)
          else
            s
    let member s e =
      let _, look = hash_and_lookup s e in
      match look with
      | None -> false
      | Some lst -> List.mem e lst
    let remove_return_list (lst : 'a list) : ('a * 'a list) =
      match lst with
      | [] -> raise (Empty_Set "Dict bucket empty")
      | hd::tl -> (hd, tl);;
    let choose s =
      let look = D.choose s in
      match look with
      | None -> raise (Empty_Set "Can't choose from an empty set")
      | Some (k, lst, d) ->
          if List.length lst = 1
          then Some (List.hd lst, d)
          else
            let ele, lstNew = remove_return_list lst in
            Some (ele, D.insert d k lstNew)
    let fold f u s =
      D.fold (fun b _ vals -> List.fold_left f b vals) u s

    let string_of_elt = C.string_of_t
    let string_of_set s = D.string_of_dict s

    (* Tests for the DictSet functor -- Use the tests from the ListSet
       functor to see how you should write tests. However, you must
       write a lot more comprehensive tests to test ALL your
       functions. *)

    let tp = print_endline

    let set_from_list (lst : elt list) : set =
      List.fold_right (fun ele s -> insert s ele) lst empty

      (* Helper functions for testing *)

    let element_list_to (max : int) : elt list =
      let rec inner (count : int) (last : elt) : elt list =
      (if count < max
      then
        let cur = C.gen_gt last in
        cur :: inner (count + 1) cur
      else []) in
        inner 0 (C.gen ())

    let recursion_doer (f : 'a -> elt) (size : int) : elt list =
      let rec inner_rec (count : int) : elt list =
        (if count < size
        then f () :: inner_rec (count + 1)
        else []) in
      inner_rec 0

    let print_set (s : set) : unit =
      print_endline (string_of_set s)

    let keys_values_list (s : set) : (D.key * D.value) list =
      D.fold (fun lst k v -> (k, v) :: lst) [] s

    let random_element_list (size : int) : elt list =
      recursion_doer C.gen_random size

    let compare_lists (lst1 : (D.key * D.value) list)
                      (lst2 : (D.key * D.value) list)
                    : bool  =
      let compare_one x y = List.for_all (fun e -> List.mem e y) x in
      compare_one lst1 lst2 && compare_one lst2 lst1

    let compare_sets (s1 : set) (s2 :set) : bool =
      compare_lists (keys_values_list s1) (keys_values_list s2)

    let overlapping_element_lists (iniSize : int) : elt list * elt list =
      let lst1 = element_list_to iniSize in
      Random.self_init ();
      List.fold_right
        (fun e (lst2, lst3) ->
          let num = Random.int 12 in
          match num with
          | 0 | 1 | 2 -> e :: lst2, lst3
          | 9 | 10 | 11 -> lst2, e :: lst3
          | _ -> e :: lst2, e :: lst3) lst1 ([], [])

    (* Test functions for DictSet.
       In addition to these test, the output of queries with ListSets and
       DictSets was compared to ensure the same results. *)

    let test_is_empty () : unit =
      let emp = empty in
      assert (is_empty emp);;

    let test_insert (lst : elt list) : unit =
      let s1 = List.fold_right (fun ele s -> insert s ele) lst empty in
      List.iter (fun e -> assert (member s1 e)) lst;;

    let test_union (lst1 : elt list) (lst2 : elt list) : unit =
      let s1, s2 = set_from_list lst1, set_from_list lst2 in
      let s3 = union s1 s2 in
      ignore s3;;

    let test_intersect () =
      let l1, l2 = overlapping_element_lists 12 in
      let s1, s2 = set_from_list l1, set_from_list l2 in
      let s3 = intersect s1 s2 in
      let l3 = List.fold_right
                (fun ele res ->
                  if List.mem ele l2
                  then ele :: res
                  else res) l1 [] in
      assert (compare_sets s3 (set_from_list l3));;

    let test_remove () =
      let vals = element_list_to 6 in
      let s1 = List.fold_right (fun e s -> insert s e) vals empty in
      let x0 = C.gen () in
      let x1 = C.gen_gt x0 in
      let xn = C.gen_lt x0 in
      let f x y = remove y x in
      f x0 s1 |> f xn |> f x1 |> ignore;;

    let test_member () =
      let vals1 = element_list_to 10 in
      let vals2 = element_list_to 8 in
      let s1 = set_from_list vals2 in
      assert (member s1 (List.nth vals1 0) &&
              member s1 (List.nth vals1 1) &&
              not (member s1 (List.nth vals1 8)) &&
              not (member s1 (List.nth vals1 9)));;

    let test_choose () =
      let vals = element_list_to 15 in
      let s0 = set_from_list vals in
      match choose s0 with
      | None -> assert (false)
      | Some (e1, s1) ->
          (match choose s1 with
          | None -> assert (false)
          | Some (e2, s2) ->
              let f x y = insert y x in
              let s3 = empty |> f e1 |> f e2 in
              assert (compare_sets (union s2 s3) s0 &&
                      compare_sets (intersect s3 s2) empty));;

    let test_fold () =
      let vals = element_list_to 7 in
      let s0 = set_from_list vals in
      let total = fold (fun x y ->C.string_of_t y ^ "-->" ^ x) "" s0 in
      ignore total;;

    (* Add your test functions to run_tests *)
    let run_tests () =
      test_is_empty ();
      test_insert (random_element_list 8);
      test_intersect ();
      test_member ();
      test_choose ();
      (* Tested by visual inspection of printout *)
      test_fold ();
      test_remove ();
      test_union (element_list_to 5) (element_list_to 10)

end
(*----------------------------------------------------------------------
  Running the tests.
 *)

(* Create a module for sets of ints using the ListSet functor and test
   it. *)
module IntListSet = ListSet(IntComparable) ;;


(* Create a set of ints using the DictSet functor and test it.

   Uncomment out the lines below when you are ready to test your set
   implementation based on dicctionaries. *)


module IntDictSet = DictSet(IntComparable) ;;

let _ = IntDictSet.run_tests();;



(*----------------------------------------------------------------------
  Make -- a functor that creates a set module by calling the ListSet
  or DictSet functors.

  This allows switching between th two implementations for all sets
  just by changing one place in the code.  *)

module Make (C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets
     when you are finished. *)
   DictSet (C)
