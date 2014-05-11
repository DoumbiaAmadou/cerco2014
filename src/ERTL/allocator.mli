
(** This module describes what are the results of a register allocation
    algorithm: a function that associates to each pseudo-register a physical
    location, and the size needed in the stack. *)

module type S = sig
  val lookup : Register.t -> ERTLToLTLI.decision
  val locals : AST.immediate
end
