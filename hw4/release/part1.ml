(* PROBLEM SET 4 - PART 1 *)
(* Name: Jeff Mu          *)
(* NetID: jm776           *)

module D = DisjointSet

(* A vertex in the graph is simply labeled by an integer index *)
type vertex = int

(* A weighted edge (u, v, w) corresponds to an edge between 
 * vertices u and v with weight w *)
type weighted_edge = vertex * vertex * float

(* Abstraction function:
 *      (n, [(u1, v1, w1), (u2, v2, w3), ...]) is a graph
 *      represented as a number of nodes and list of edges
 *      with vertex set {0, 1, ..., n - 1}
 * and edge set {{u1, v1}, {u2, v2}, ...},
 *      with edges weighted by the function w(ui, vi) = wi
 *)
type graph = int * weighted_edge list

(* Kruskal's Algorithm takes a number of nodes and a list of
 * edges representing an undirected graph and returns the
 * edges which make up the minimal spanning tree (or forest) *)
let kruskal (g : graph) : (weighted_edge list) = 
  let n,l = g in
  let u = D.createUniverse n in
  let compare_edge e1 e2 = 
    let _,_,w1 = e1
    and _,_,w2 = e2 in
    compare w1 w2 in
  let sorted_edges = List.rev (List.sort compare_edge l) in
  let rec helper edges mst = 
    match edges with
      | [] -> mst
      | h::t -> let (n1,n2,_) = h in 
								let t1 = D.find u n1 
								and t2 = D.find u n2 in
								if t1 = t2 then helper t mst
								else (D.union u n1 n2 ; helper t (h::mst)) in
  helper sorted_edges []

(* List of contiguous integers: 1 <|> 5 = [1; 2; 3; 4; 5] *)
let (<|>) x y =
  let rec loop i acc = if i > y then List.rev acc else loop (i+1) (i::acc) in
  loop x []
 
(* Estimates the percolation threshold for an nxn grid using the
 * algorithm described in the writeup.
 * Requires: n > 2 *)
let calc_p_threshold (n:int) : float =
  Random.self_init ();
  let s = n*n in
  let u = D.createUniverse s and u2 = D.createUniverse s in
  let top = (<|>) 0 (n-1) and bot = (<|>) (s-n) (s-1) 
  (* shuffle occupied list so we don't have to worry about random repeats *)
  (* selfnote:List.sort (fun _ _ ->(Random.int 3)-1) is not a uniform shuffle *)
  and occupied = 
    let occupied_array = Array.of_list ((<|>) n (s-n-1)) in
    let swap a i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t in
    let shuffle a = Array.iteri (fun i _ -> swap a i (Random.int (i+1))) a in
    shuffle occupied_array;
    Array.to_list occupied_array in
  (* initialize the disjoint set for percolation testing *)
  ignore (List.map (D.union u 0) top); ignore (List.map (D.union u (s-1)) bot);
  (* initialize the disjoint set for vanact nodes *)
  ignore (List.map (D.union u2 0) top); ignore (List.map (D.union u2 0) bot);
  (* check surrounding nodes for vacancies *)
  let make_vacant i = 
    let up = D.find u (i-n) and dn = D.find u (i+n) 
    and lt = D.find u (i-(if (i mod n) = 0 then 0 else 1)) 
    and rt = D.find u (i+(if (i mod n) = (n-1) then 0 else 1)) in
    if ((D.find u2 up) = (D.find u2 0)) then D.union u up i ;
		if ((D.find u2 dn) = (D.find u2 0)) then D.union u dn i ;
		if ((D.find u2 lt) = (D.find u2 0)) then D.union u lt i ;
		if ((D.find u2 rt) = (D.find u2 0)) then D.union u rt i ;
    () in
  (* randomly unoccupy a node until percolation *)
	let rec helper l = 
    if ((D.find u 0) = (D.find u (s-1))) then 
      float_of_int(D.size u2 0)/.float_of_int(s)
    else 
      match l with 
        | h::t -> D.union u2 0 h ; make_vacant h ;
                  helper t
        | [] -> failwith ("Failure: grid vacant, but no percolation found")
  in helper occupied

(* Averages the percolation threshold for an nxn grid
 * over i trials *)  
let average_p_threshold (n : int) (i : int) : float =
  let runs = List.map (fun _ -> calc_p_threshold n) (1 <|> i) in
 (List.fold_left (+.) 0. runs) /. (float_of_int i)
