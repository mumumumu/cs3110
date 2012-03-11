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
  let compare_edge e1 e2 = 
    let _,_,w1 = e1
    and _,_,w2 = e2 in
    compare w1 w2 in
  let sorted_edges = List.rev (List.sort compare_edge l) in
  let rec connected lst e = 
    match lst with
      | [] -> false
      | h::t -> let (v1,v2,_) = h 
                and (n1,n2,_) = e in
                if v1=n1 && v2=n2 then true
                if v1=n1 then connected t 
                else connected t v1 n1 || connected t v2 n2 in
  let rec helper edges mst = 
    match edges with
      | [] -> mst
      | h::t -> if connected mst h then helper t mst
                else helper t (h::mst) in
  (n, helper sorted_edges [])
    
(* List of contiguous integers: 1 <|> 5 = [1; 2; 3; 4; 5] *)
let (<|>) x y =
  let rec loop i acc = if i > y then List.rev acc else loop (i+1) (i::acc) in
  loop x []
 
(* Estimates the percolation threshold for an nxn grid using the
 * algorithm described in the writeup.
 * Requires: n > 2 *)
let calc_p_threshold (n:int) : float =
  failwith "not implemented"
  
(* Averages the percolation threshold for an nxn grid
 * over i trials *)  
let average_p_threshold (n : int) (i : int) : float =
  let runs = List.map (fun _ -> calc_p_threshold n) (1 <|> i) in
  (List.fold_left (+.) 0. runs) /. (float_of_int i)