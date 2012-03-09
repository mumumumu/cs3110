type weighted_edge = int * int * float
type graph = int * weighted_edge list

(* Represents a partition of the graph into connected components *)
type segmentation

(* segment g k
 * Segments the graph g using k as the constant parameter
 * used in the threshold function.  Returns a graph segmentation
 * partitioning the graph into connected components *)
val segmentGraph : graph -> float -> segmentation

(* mergeSmallComponents g min_size
 * Merges components whose size is less than min_size with
 * neighboring components *)
val mergeSmallComponents : segmentation ->
	weighted_edge list -> int -> segmentation

(* colorComponents img seg returns a new image such that each segment
 * is colored with the average color in that segment. Any pixel that
 * borders a different segment is colored white. *)
val colorComponents: Image.image -> segmentation -> Image.image
  
(* segmentImage img sigma k minSize Segments img using parameters:
 * sigma for Gaussian smoothing parameter, k for the threshold
 * function, and minSize for the merging threshold. Returns a new image
 * such that each segment is colored according to the average of
 * all pixels in that segment. *)
val segmentImage : Image.image -> float -> float -> int -> segmentation

(* components g s
 * Returns [c1, c2, ..., cN] where each ci is a list containing
 * the vertices of one of the N components of the graph. *)
val components : graph -> segmentation -> int list list

(* Segments the image stored in the bitmap file located at img_filename
 * with the given parameters. Saves and shows the results *)
val segmentAndShow : string -> float -> float -> int -> unit
