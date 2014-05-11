
(** This module describes the register allocation process by coloring. *)

module type G = sig
  val fun_name : string
  val graph : Interference.graph
  val uses : Register.t -> int
  val verbose : bool
end

module Make (G : G) = struct

  (* Build an interference graph for this function, and color
     it. Define a function that allows consulting the coloring. *)

  module C = Coloring.Color (G)

  let lookup r =
    Interference.Vertex.Map.find (Interference.lookup G.graph r) C.coloring

    (* Restrict the interference graph to concern spilled vertices only,
       and color it again, this time using stack slots as colors. *)

  module H = struct
    let graph = Interference.droph (Interference.restrict G.graph (fun v ->
      match Interference.Vertex.Map.find v C.coloring with
	| Coloring.Spill ->
	  true
	| Coloring.Color _ ->
	  false
    ))
    let verbose = false
    let () =
      if verbose then
	Printf.printf "Starting stack slot allocation for %s.\n" G.fun_name
  end

  module S = Spill.Color (H)

    (* Define a new function that consults both colorings at once. *)

  let lookup r =
    match lookup r with
      | Coloring.Spill ->
	ERTLToLTLI.Spill (Interference.Vertex.Map.find (Interference.lookup H.graph r) S.coloring)
      | Coloring.Color color ->
	ERTLToLTLI.Color color

  let locals = S.locals

end
