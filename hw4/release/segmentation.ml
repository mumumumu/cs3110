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
type segmentation = int

(* segmentGraph g k segments g using the parameter
 * k for the threshold function
 * Returns: a segmentation of the graph *)
let segmentGraph ((n, wedges) : graph) (k : float) : segmentation =
  failwith "not implemented"

let mergeSmallComponents (u : segmentation) (edges : weighted_edge list)
		(minSize : int) : segmentation =
  failwith "not implemented"

let segmentImage (img : I.image) (sigma : float) (k : float)
		(minSize : int) : segmentation =
	failwith "not implemented"
  
let colorComponents (img : I.image) (u : segmentation) : I.image =
  failwith "not implemented"
    
let components ((n, _) : graph) (s : segmentation) : int list list =
  failwith "not implemented"
  
(* Segments the image stored in the bitmap file located at img_filename
 * with the given parameters. Saves and shows the results *)
let segmentAndShow (img_filename : string) (sigma : float)
      (k : float) (min_size : int) : unit =
  let img = I.load img_filename in
  let seg = segmentImage img sigma k min_size in
  let colored = colorComponents img seg in
    I.save colored "output/segmented.bmp";
    I.show colored
