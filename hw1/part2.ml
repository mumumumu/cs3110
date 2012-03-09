(* Name: Jeff Mu *)
(* NetID: jm776  *) 
(* Section: 204  *)

(* Questions 2 solution: *)					
(* Applies f to all elements in lst *)
let rec zardoz f lst = 
  match lst with
    | [] -> []
    | h::t -> (f h)::(zardoz f t)
