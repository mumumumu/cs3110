#use "disjointSet.ml";;
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
  let u = createUniverse n in
  let compare_edge e1 e2 = 
    let _,_,w1 = e1
    and _,_,w2 = e2 in
    compare w1 w2 in
  let sorted_edges = List.rev (List.sort compare_edge l) in
  let rec helper edges mst = 
    match edges with
      | [] -> mst
      | h::t -> let (n1,n2,_) = h in 
								let t1 = find u n1 
								and t2 = find u n2 in
								if t1 = t2 then helper t mst
								else let _ = union u n1 n2 in helper t (h::mst) in
  helper sorted_edges []

let kruskal_test =
  kruskal (2,[(0,1,2.0);(1,0,3.0)]);
  kruskal (3,[(0,1,1.0);(1,2,1.0);(2,0,3.0)]);
  kruskal (5,[(0,1,1.0);(1,2,1.0);(2,3,1.0);(3,4,1.0);(4,0,1.0);(1,3,1.0);
							(1,4,1.0);(2,4,1.0);(2,0,1.0);(3,0,1.0)])

(* List of contiguous integers: 1 <|> 5 = [1; 2; 3; 4; 5] *)
let (<|>) x y =
  let rec loop i acc = if i > y then List.rev acc else loop (i+1) (i::acc) in
  loop x []
 
(* Estimates the percolation threshold for an nxn grid using the
 * algorithm described in the writeup.
 * Requires: n > 2 *)
let calc_p_threshold (n:int) : float =
  let s = n*n in
  let u = createUniverse s in
  let top = (<|>) 0 (n-1)
  and bot = (<|>) (s-n) (s-1) 
  and occupied = (<|>) n (s-n-1) in
  let shuffled = List.sort (fun a b -> (Random.int 3) - 1) occupied in 
  let _ = List.map (union u 0) top in
  let _ =  List.map (union u (s-1)) bot in
  let perc i v = 
    let help i =
      let t = find u 0 
      and b = find u (s-1) in
      let up = find u (i-n)
      and dn = find u (i+n) 
      and lt = find u (i-(if (i mod n) = 0 then 0 else 1)) 
      and rt = find u (i+(if (i mod n) = (n-1) then 0 else 1)) in
      (*let _ = print_string ((string_of_int t) ^ " " ^ (string_of_int b) ^ " " ^
      (string_of_int up) ^ " " ^ (string_of_int dn) ^ " " ^ (string_of_int lt) ^ " " ^
      (string_of_int rt) ^ "\n") in*)
      let _ = if (t = up || t = dn || t = lt || t = rt) then union u 0 i in
      let _ = if (b = up || b = dn || b = lt || b = rt) then union u (s-1) i in
      (t = up || t = dn || t = lt || t = rt) && (b = up || b = dn || b = lt || b = rt) in
    let nv = List.fold_left (fun x h -> if (help h) then x else (h::x)) [] (i::v) in
    (((find u 0) = (find u (s-1))),nv) in
	let rec helper b l v = 
    if b then float_of_int(size u 0)/.float_of_int(s)
    else let h::t = l in 
			   (*let _ = print_string ((string_of_int h) ^ "\n") in*)
         let b1,v1 = perc h v in
         helper b1 t v1
  in helper false shuffled []
 
(* Averages the percolation threshold for an nxn grid
 * over i trials *)  
let average_p_threshold (n : int) (i : int) : float =
  let runs = List.map (fun _ -> calc_p_threshold n) (1 <|> i) in
  (List.fold_left (+.) 0. runs) /. (float_of_int i)
