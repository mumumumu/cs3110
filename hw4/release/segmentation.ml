(* PROBLEM SET 4 - PART 2 *)
(* Name: Jeff Mu          *)
(* NetID: jm776           *)

module I = Image
module A = Array
module D = DisjointSet

(* (u, v, w) is the edge {u, v} with weight w *)
type weighted_edge = int * int * float

(* Abstraction function:
 *   (n, [(u1, v1, w1), (u2, v2, w3), ...]) is a graph
 *   with vertex set {0, 1, ..., n - 1}
 * and edge set {{u1, v1}, {u2, v2}, ...},
 *   with edges weighted by the function w(ui, vi) = wi   *)
type graph = int * weighted_edge list

(* Represents a partition of the graph into connected components *)
type segmentation = DisjointSet.universe

(* sorts a list of edges in increasing order by weight*)
let sort_edges edges = 
  let cmp e1 e2 = let _,_,w1 = e1 and _,_,w2 = e2 in compare w1 w2 in
  List.sort cmp edges

(* segmentGraph g k segments g using the parameter
 * k for the threshold function
 * Returns: a segmentation of the graph *)
let segmentGraph ((n, wedges) : graph) (k : float) : segmentation =
  let s = D.createUniverse n in
  let t_c v = k/.(float_of_int v) in
  (* initial array of threshold values for each 1-node component *)
  let threshold = Array.make n (t_c 1) in
  let rec helper e_lst =
    match e_lst with 
      | [] -> s
      | h::t -> 
          let n1,n2,w = h in
          let a = (D.find s n1) and b = (D.find s n2) in
          let _ = if (a != b) && (w <= (min threshold.(a) threshold.(b))) then
            let _ = D.union s a b in
            let q = D.find s a in
            (* update threshold value for root node *)
            threshold.(q) <- (w +. (t_c (D.size s q))); () in
          helper t
  in helper (sort_edges wedges)

let mergeSmallComponents (u : segmentation) (edges : weighted_edge list)
    (minSize : int) : segmentation =
  let rec helper e_lst =
    match e_lst with
      | [] -> u 
      | h::t -> 
          let n1,n2,w = h in
          let a = (D.find u n1) and b = (D.find u n2) in
          if (a != b) && ((D.size u a) < minSize || (D.size u b) < minSize) then
            D.union u a b;
          helper t
  in helper (sort_edges edges)
  
let segmentImage (img : I.image) (sigma : float) (k : float)
    (minSize : int) : segmentation =
  let simg = I.smooth sigma img in
  let w = I.width simg and h = I.height simg in
  let diff x1 y1 x2 y2 = 
    let r1,g1,b1 = (I.getPixel simg x1 y1) 
    and r2,g2,b2 = (I.getPixel simg x2 y2) in
    sqrt (((r1-.r2)**2.0) +. ((g1-.g2)**2.0) +. ((b1-.b2)**2.0)) in
  (* create list of edges by folding across the image *)
  let edges = I.fold (fun x y c l ->
    let l1 = if x<w-1 then (y*w+x,y*w+x+1,diff x y (x+1) y)::l 
      else l in
    let l2 = if y<h-1 then (y*w+x,(y+1)*w+x,diff x y x (y+1))::l1
      else l1 in
    let l3 = if x>0 then (y*w+x,y*w+x-1,diff x y (x-1) y)::l2 
      else l2 in
    let l4 = if y>0 then (y*w+x,(y-1)*w+x,diff x y x (y-1))::l3 
      else l3 in
    let l5 = if x<w-1 && y<h-1 then (y*w+x,(y+1)*w+x+1,diff x y (x+1) (y+1))::l4
      else l4 in
    let l6 = if x>0 && y<h-1 then (y*w+x,(y+1)*w+x-1,diff x y (x-1) (y+1))::l5
      else l5 in
    let l7 = if x<w-1 && y>0 then (y*w+x,(y-1)*w+x+1,diff x y (x+1) (y-1))::l6 
      else l6 in
    let l8 = if x>0 && y>0 then (y*w+x,(y-1)*w+x-1,diff x y (x-1) (y-1))::l7 
      else l7 in
    l8) [] simg in
  let seg = segmentGraph (w*h,edges) k in
  mergeSmallComponents seg edges minSize
  
let colorComponents (img : I.image) (u : segmentation) : I.image =
  let w = I.width img and h = I.height img in
  let colors = Array.make (w*h) (0.0,0.0,0.0) in
  (* sum up all color values in each component *)
  let _ = I.fold (fun x y c _ -> 
                    let i = D.find u (w*y+x) in
                    let r1,g1,b1 = colors.(i) and r2,g2,b2 = c in
                    colors.(i) <- (r1+.r2,g1+.g2,b1+.b2)) () img in
  (* average color values for each component *)
  let avg_colors = Array.mapi 
      (fun i a -> let r,g,b = a and n = float_of_int (D.size u i) in
      (r/.n,g/.n,b/.n))  colors in
  (* create new image with new colors *)
  let new_img = I.map (fun x y c -> avg_colors.(D.find u (w*y+x))) img in
  (* add white border between components *)
  I.map (fun x y c -> let n = D.find u (w*y+x) in
           let up = D.find u (if y<h-1 then (w*(y+1)+x) else n)
           and dn = D.find u (if y>0 then (w*(y-1)+x) else n)
           and rt = D.find u (if x<w-1 then (w*y+x+1) else n)
           and lt = D.find u (if x>0 then (w*y+x-1) else n)
           and dr = D.find u (if y>0 && x<w-1 then (w*(y-1)+x+1) else n)
           and dl = D.find u (if y>0 && x>0 then (w*(y-1)+x-1) else n)
           and ur = D.find u (if y<h-1 && x<w-1 then (w*(y+1)+x+1) else n)
           and ul = D.find u (if y<h-1 && x>0 then (w*(y+1)+x-1) else n) in
    if n != up || n != dn || n != rt || n != lt || 
       n != dr || n != dl || n != ur || n != ul then (255.0,255.0,255.0) 
    else c) new_img

let components ((n, _) : graph) (s : segmentation) : int list list =
  let comp = Array.make n [] in
  let rec helper n = 
    (* create an array of lists, adding nodes to the root node they belong to *)
    if n >= 0 then 
      let i = D.find s (n) in
      let _ = comp.(i) <- ((n+1)::(comp.(i))) in
      helper (n-1) 
    else 
      List.fold_left
        (fun l x -> if x != [] then x::l else l) [] (Array.to_list comp)
  in helper (n-1)
      
(* Segments the image stored in the bitmap file located at img_filename
 * with the given parameters. Saves and shows the results *)
let segmentAndShow (img_filename : string) (sigma : float)
      (k : float) (min_size : int) : unit =
  let img = I.load img_filename in
  let seg = segmentImage img sigma k min_size in
  let colored = colorComponents img seg in
    I.save colored "output/segmented.bmp";
    I.show colored

let _ = segmentAndShow "images/beach_original.bmp" 0.5 500. 50