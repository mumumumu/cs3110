module I = Image
module A = Array

(* (u, v, w) is the edge {u, v} with weight w *)
type weighted_edge = int * int * float

(* Abstraction function:
 * 	(n, [(u1, v1, w1), (u2, v2, w3), ...]) is a graph
 * 	with vertex set {0, 1, ..., n - 1}
 * and edge set {{u1, v1}, {u2, v2}, ...},
 * 	with edges weighted by the function w(ui, vi) = wi   *)
type graph = int * weighted_edge list

(* Represents a partition of the graph into connected components *)
(* TODO: Define this! *)
type segmentation = int * disjointSet

(* segmentGraph g k segments g using the parameter
 * k for the threshold function
 * Returns: a segmentation of the graph *)
let segmentGraph ((n, wedges) : graph) (k : float) : segmentation =
  let s = createUniverse n in
  let sort_edges e_lst = 
    let cmp e1 e2 = let _,_,w1 = e1 and _,_,w2 = e2 in compare w1 w2 in
    List.sort cmp e_lst in
  let t_c nc = k/.(float_of_int nc) in
  let threshold = Array.make n (t_c n) in
  let rec helper nc e_lst =
    match el with 
      | [] -> n,s
      | h::t -> let n1,n2,w = h in
                let a = find s n1 and b = find s n2 in
                if a != b && w <= thershold.(a) && w <= threshold.(b) then
                  union s a b;
                  threshold.(a) <- w + t_c(size u a);
                  helper n-1 t
                else 
                  helper n t
  in helper n (sort_edges wedges)

let mergeSmallComponents (u : segmentation) (edges : weighted_edge list)
		(minSize : int) : segmentation =
  let rec helper e_lst (n,s) =
    match e_lst with
      | [] -> n,s
      | h::t -> let n1,n2,w = h in
                let a = find s n1 and let b = find s n2 in
                if ((a != b) && ((size s a < minSize) || size s b < min_size))) then
                  union s a b
                  helper t (n-1,s)
                else 
                  helper t (n,s)
  in helper edges u

let segmentImage (img : I.image) (sigma : float) (k : float)
		(minSize : int) : segmentation =
  let simg = I.smooth sigma img in
  let w = width simg and h = height simg in
  let diff x1 y1 x2 y2 = 
    let r1,g1,b1 = (getPixel x1 y1) and r2,g2,b2 = (getPixel x2 y2) in
    sqrt (((r1-r2)**2) + ((g1-g2)**2) + ((b1-b2)**2))
  let edges = I.fold (fun x y c l ->
    let l1 = if x<w-1 then (y*w+x,y*w+x+1,diff x y (x+1) y)::l else l in
    let l2 = if y<h-1 then (y*w+x,(y+1)*w+x,diff x y x (y+1))::l1 else l1 in
    let l3 = if x>0 then (y*w+x,y*w+x-1,diff x y (x-1) y)::l2 else l2 in
    let l4 = if y>0 then (y*w+x,(y-1)*w+x,diff x y x (y-1))::l3 else l3 in
    let l5 = if x<w-1 && y<h-1 then (y*w+x,(y+1)*w+x+1,diff x y (x+1) (y+1))::l4 else l4 in
    let l6 = if x>0 && y<h-1 then (y*w+x,(y+1)*w+x-1,diff x y (x-1) (y+1))::l5 else l5 in
    let l7 = if x<w-1 && y>0 then (y*w+x,(y-1)*w+x+1,diff x y (x+1) (y-1))::l6 else l6 in
    let l8 = if x>0 && y>0 then (y*w+x,(y-1)*w+x-1,diff x y (x-1) (y-1))::l7 else l7 in
    l8) [] simg in
  let seg = segmentGraph (w*h,edges) k in
  mergeSmallComponents seg edges minSize
  
let colorComponents (img : I.image) (u : segmentation) : I.image =
  let _,s = u in
  let w = width img and h = height img in
  let colors = Array.make (w * h) (0.0,0.0,0.0) in
  let _ = I.fold (fun x y c _ -> 
    let n = (w * y) + x in 
    let r1,g1,b1 = colors.(find s n) and r2,g2,b2 = c in
    colors.(find s n) <- (r1+r2,g1+g2,b1+b2)) _ img;
    Array.mapi (fun i a -> 
      let r,g,b = a and n = float_of_int (size s i) in
      (r/.i,g/.i,b/.i)) a;
  I.fold (fun x y _ _ -> setPixel x y (colors.(find s n)) _ (make w h)
  failwith "not implemented"
    
let components ((n, _) : graph) (s : segmentation) : int list list =
  let nc,u = s in
  let comp = Array.make nc [] in
  let rec helper n = 
    if n > 0 then 
      comp.(find u n) <- (n::(comp.(find u n)));
      helper n-1
    else List.fold_left (fun l x -> if x != [] then x::l else l) [] (Array.to_list comp)
  in helper n []
  
(* Segments the image stored in the bitmap file located at img_filename
 * with the given parameters. Saves and shows the results *)
let segmentAndShow (img_filename : string) (sigma : float)
      (k : float) (min_size : int) : unit =
  let img = I.load img_filename in
  let seg = segmentImage img sigma k min_size in
  let colored = colorComponents img seg in
    I.save colored "output/segmented.bmp";
    I.show colored
