
(** This module describes the register allocation process by coloring. *)

module type G = sig
  val fun_name : string
  val graph : Interference.graph
  val uses : Register.t -> int
  val verbose : bool
end

module Make (G : G) : Allocator.S
