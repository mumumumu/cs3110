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
let kruskal (g : graph) : (weighted_edge list)= 
  failwith "not implemented"
    
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