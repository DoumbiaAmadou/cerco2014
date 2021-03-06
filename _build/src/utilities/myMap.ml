(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: myMap.ml,v 1.3 2006/02/17 16:19:52 pottier Exp $ *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val mem:  key -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    type interval = key option * key option
    val split: interval -> 'a t -> 'a t
    val minimum: 'a t -> key * 'a
    val find_remove: key -> 'a t -> 'a * 'a t
    val update: key -> ('a -> 'a) -> 'a t -> 'a t
    val restrict: (key -> bool) -> 'a t -> 'a t
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, x, data, r, h)
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add v d r
      | (_, Empty) -> add v d l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

  (* Intervals for splitting. An interval consists of a lower bound
     and an upper bound, each of which can be absent. A key is
     considered to lie within the interval if it is both greater than
     (or equal to) the lower bound (if present) and less than (or
     equal to) the upper bound (if present). *)

  type interval =
      key option * key option

  (* Splitting. split interval m returns a new map consisting of
     all bindings in m whose keys are within interval. *)

  let rec split ((lo, hi) as interval) = function
      Empty ->
        Empty
    | Node(l, v, d, r, _) ->
	let clo = Ord.compare v lo in
	if clo < 0 then
	  (* v < lo *)
	  split interval r
	else if clo = 0 then
	  (* v = lo *)
	  add v d (splithi hi r)
	else
	  (* v > lo *)
	  let chi = Ord.compare v hi in
	  if chi < 0 then
	    (* v < hi *)
	    join (splitlo lo l) v d (splithi hi r)
	  else if chi = 0 then
	    (* v = hi *)
	    add v d (splitlo lo l)
	  else
	    (* v > hi *)
	    split interval l

  and splitlo lo = function
      Empty ->
        Empty
    | Node(l, v, d, r, _) ->
	let c = Ord.compare v lo in
	if c < 0 then
	  (* v < lo *)
	  splitlo lo r
	else if c = 0 then
	  (* v = lo *)
	  add v d r
	else
	  (* v > lo *)
	  join (splitlo lo l) v d r

  and splithi hi = function
      Empty ->
        Empty
    | Node(l, v, d, r, _) ->
	let c = Ord.compare v hi in
	if c < 0 then
	  (* v < hi *)
	  join l v d (splithi hi r)
	else if c = 0 then
	  (* v = hi *)
	  add v d l
	else
	  (* v > hi *)
	  splithi hi l

  (* Splitting. This is the public entry point. *)

  let split interval m =
    match interval with
    | None, None ->
	m
    | Some lo, None ->
	splitlo lo m
    | None, Some hi ->
	splithi hi m
    | Some lo, Some hi ->
	split (lo, hi) m

  (* Finding the minimum key in a map. *)

  let rec minimum key data m =
    match m with
    | Empty ->
	(key, data)
    | Node (l, k, d, _, _) ->
	minimum k d l

  let minimum = function
    | Empty ->
	raise Not_found
    | Node (l, k, d, _, _) ->
	minimum k d l

  (* Finding an element and removing it in one single traversal. *)

  let find_remove x m =
    let data = ref None in
    let rec remove = function
      | Empty ->
	  raise Not_found
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then begin
	    data := Some d;
            merge l r
	  end
          else if c < 0 then
            bal (remove l) v d r
          else
            bal l v d (remove r)
    in
    let m = remove m in
    match !data with
    | None ->
	assert false
    | Some d ->
	d, m

  (* Updating the data associated with an element in one single traversal. *)

  exception Unmodified

  let rec update x f m =
    let rec update = function
      | Empty ->
          assert false
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
	    let d' = f d in
	    if d == d' then
	      raise Unmodified
	    else
	      Node (l, v, d', r, h)
          else if c < 0 then
	    Node (update l, v, d, r, h)
	  else
	    Node (l, v, d, update r, h)
    in
    try
      update m
    with Unmodified ->
      m

  (* Restricting the domain of a map. *)

  let restrict p m =
    fold (fun x d m ->
      if p x then
        add x d m
      else
        m
    ) m empty


end
