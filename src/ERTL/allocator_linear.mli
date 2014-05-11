
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

module Make (G : G) : Allocator.S
