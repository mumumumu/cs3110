(* (r, g, b) is a color with r, g, and b as its red, green, and blue
 * components respectively where 0.0 <= r, g, b <= 255.0 *)
type color = float * float * float

(* an image *)
type image

(* width of the image, in pixels *)
val width : image -> int

(* height of the image, in pixels *)
val height : image -> int

(* iter f img calls (f x y c) for each pixel, where x and y are the
 * location of the pixel and c is its color *)
val iter : (int -> int -> color -> unit) -> image -> unit

(* fold f accum img returns f (0 0 c (f x y c (... (f (w-1) (h-1) c
 * accum)))) for each pixel in the image, where x and y are the
 * location of a particular pixel and c is its color *)
val fold : (int -> int -> color -> 'a -> 'a) -> 'a -> image -> 'a

(* clone img returns a new, exact copy of img *)
val clone : image -> image

(* map f img returns a new image such that pixel x, y in the new
 * image is (f x y c), where c is the color of the pixel
 * in the original image *)
val map : (int -> int -> color -> color) -> image -> image

(* Side effect: setPixel x y c sets pixel x, y to the color c
 * Checks: x and y are in bounds *)
val setPixel : image -> int -> int -> color -> unit

(* getPixel img x y returns the color of the pixel at x, y.
 * Checks: x and y are in bounds *)
val getPixel : image -> int -> int -> color

(* load f returns an image containing the bitmap file named f.
 * Requires: f is a 24-bit bitmap *)
val load : string -> image

(* save img f saves img as a 24-bit bitmap file named f *)
val save : image -> string -> unit

(* Displays an image in a window *)
val show : image -> unit

(* smooth sigma img returns a new image obtained by performing
 * Gaussian smoothing on img with parameter sigma.
 * Requires: sigma >= 0 *)
val smooth : float -> image -> image

(* make w h returns a new w by h pixel image
 * Requires: w, h >= 0 *)
val make : int -> int -> image
