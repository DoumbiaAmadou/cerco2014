
(** This module describes the register allocation process by linear scan of the
    live intervals of the pseudo-registers. *)

module type G = sig
  (* Entry label of the ERTL function *)
  val entry : Label.t
  (* Successors of a label in the graph of the ERTL function *)
  val succs : Label.t -> Label.t list
  (* Pseudo-registers of the function *)
  val pseudo_registers : Register.t list
  (* Live pseudo-registers at a given label *)
  val valuation : Label.t -> Liveness.L.t
  (* Statement from label *)
  val stmt : Label.t -> ERTL.statement
end

module Make (G : G) = struct
  module Interval = struct
  type range = {mutable rbegin: int; mutable rend: int;}

(* interval for a register but can allocate many times *)
  type interval = 
  {
     mutable reg: Register.t;
     mutable ibegin: int;
     mutable iend: int;
     mutable ranges: range list;
  } 

(* Check if two intervals overlap *)
  let overlapping_ranges r0 r1 = r0.rend > r1.rbegin && r1.rend > r0.rbegin 

  let overlapping i0 i1 = 
    let rec test_ranges r0s r1s = 
      begin match r0s, r1s with
        |[], _ -> false
		|_, [] -> false
		|r0hd::r0tl, r1hd::r1tl -> 
			if overlapping_ranges r0hd r1hd then true
			else if r0hd.rend < r1hd.rend then test_ranges r0tl r1s
			else if r0hd.rend > r1hd.rend then test_ranges r0s r1tl
			else test_ranges r0tl r1tl
	  end
    in test_ranges i0.ranges i1.ranges

  let live_on i p = 
  let rec live_on_ranges r =
    begin match r with
    | [] -> false
    | hd::tl -> 
          if p < hd.rbegin then false
          else if p < hd.rend then true
          else live_on_ranges tl
    end in
  live_on_ranges i.ranges  
  
  let remove_expired_ranges i pos =
  let rec filter = function
    [] -> []
	| r :: rl' as rl -> if pos < r.rend then rl
               else filter rl' in
				i.ranges <- filter i.ranges

 end 

 module LinearScan = struct
   open Interval
   open List

   let number_registre = 2

   type active_t = 
   {
	mutable active: interval list; (* active register *)
	mutable free: interval list; (* free register *)
   }

(* Insert interval into list sorted by increasing end position *)
   let rec insert_interval_sorted i = function
     [] -> [i]
     |hd::_ as ls when hd.iend <= i.iend -> i::ls
     |hd::tl -> hd::insert_interval_sorted i tl

   let rec release_expired_active ci pos = function
     hd::tl when hd.iend >= pos -> remove_expired_ranges hd pos;
     if live_on hd pos then hd::release_expired_active ci pos tl
     else begin ci.free <- insert_interval_sorted hd ci.free;
        release_expired_active ci pos tl
     end 
    |_ -> []
 
 end

  let local = ref 30

  let lookup (r : Register.t) : ERTLToLTLI.decision = ERTLToLTLI.Spill 10
  (*   assert false (* TODO M1 *) *)

  let locals : int = !local

end
