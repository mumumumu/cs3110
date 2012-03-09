open Bmp
open Images
module G = Graphics
module A = Array

type color = float * float * float
type image = (color array array) * int * int

let width ((_,w,_) : image) = w

let height ((_,_,h) : image) = h

let setPixel ((contents,_,_) : image) (x : int) (y : int) (c : color) : unit =
	A.set (A.get contents y) x c

let getPixel ((contents,_,_) : image) (x : int) (y : int) : color =
	A.get (A.get contents y) x

let make (w : int) (h : int)=
	(A.make_matrix h w (0.0, 0.0, 0.0), w, h)

let fold (f : int -> int -> color -> 'a -> 'a) (accum : 'a) (img : image) : 'a =
	let w, h = width img, height img in
	let rec fold_row x y accum =
		let accum' = f x y (getPixel img x y) accum in
			if x + 1 < w then fold_row (x+1) y accum'
			else if y + 1 < h then fold_row 0 (y+1) accum'
			else accum' in
		fold_row 0 0 accum

let iter (f : int -> int -> color -> unit) (img : image) : unit =
	fold (fun x y c _ -> f x y c) () img

let clone ((contents, w, h) : image) : image =
	((A.map (fun row -> A.copy row) contents), w, h)

let rawColor img x y =
  let e = Rgb24.get img x y in
	let r = e.Color.r in
	let g = e.Color.g in
	let b = e.Color.b in
		(float r, float g, float b)

let map (f : int -> int -> color -> color) ((_, w, h) as img : image) : image =
	let img' = clone img in
		iter (fun x y c -> setPixel img' x y (f x y (getPixel img' x y))) img';
		img'

let load (filename : string) : image =
	let imageOfRgb24 raw =
		let w = raw.Rgb24.width in
		let h = raw.Rgb24.height in
		let img = make w h in
			iter (fun x y _ -> setPixel img x y (rawColor raw x y)) img;
			img in
		match (Bmp.load filename [Load_Progress(fun y -> ())]) with
				Rgb24 x -> imageOfRgb24 x
			| _ -> failwith "Not a BMP (RGB24) image"

let save (img : image) (filename : string) : unit =
	let rgbImg = Rgb24.create (width img) (height img) in
		iter (fun x y (r,g,b) ->
			let color = {Color.r = int_of_float r;
									 Color.g = int_of_float g;
									 Color.b = int_of_float b} in
				Rgb24.set rgbImg x y color) img;
		Bmp.save filename [] (Rgb24 rgbImg)

let winDimsOfImage ((_, w, h) : image) =
	(* the dimensions passed to open_graph do
	 * not account for the window's toolbar, so just guess its size *)
	let toolbarSize = 35 in
		(w, h + toolbarSize)

(* converts the given image into a format the OCaml graphics library
 * can display *)
let graphicsImage ((contents, _, _) : image) : Graphics.image =
	let pixels = A.map
		(A.map (fun (r,g,b) -> Graphics.rgb (int_of_float r)
			(int_of_float g) (int_of_float b))) contents in
		G.make_image pixels

let show (img : image) =
	let w, h = winDimsOfImage img in
		G.close_graph ();
		G.open_graph (Printf.sprintf " %dx%d" w h);
		G.draw_image (graphicsImage img) 0 0
    
(* make the values in the filter sum to 1 *)
let normalize mask =
	let sum = A.fold_left (+.) 0.0 mask in
	let norm = 2.0 *. sum -. (A.get mask 0) in
		A.map (fun x -> x /. norm) mask

(* creates a 1-dimensional Gaussian filter with paramater sigma *)
let gaussianFilter (sigma : float) : float array =
	let sigma = max sigma 0.01 in
	let width = 4.0 in
	let len = 1 + int_of_float (ceil(sigma *. width)) in
	let sq x = x *. x in
	let filter = A.init len
		(fun i -> exp(-0.5 *. (sq (float (i) /. sigma)))) in
		normalize filter

(* used with separable filters such as the Gaussian *)
let convolveAndTranspose mask img =
	let w = width img in
	let h = height img in
	let img' = make h w in
		for y = 0 to h - 1 do
			for x = 0 to w - 1 do
				let sum = [|0.0; 0.0; 0.0|] in
					for i = 0 to A.length mask - 1 do
						let r1,g1,b1 = getPixel img (min (w-1) (x+i)) y in
							(* avoid double-counting mask.(0) *)
						let r2,g2,b2 =
							if i = 0 then 0.0, 0.0, 0.0
							else getPixel img (max 0 (x-i)) y in
							A.set sum 0 (sum.(0) +. mask.(i) *. (r1 +. r2));
							A.set sum 1 (sum.(1) +. mask.(i) *. (g1 +. g2));
							A.set sum 2 (sum.(2) +. mask.(i) *. (b1 +. b2));
					done;
					(* y and x are backwards *)
					setPixel img' y x (sum.(0), sum.(1), sum.(2))
			done;
		done;
		img'

(* Please see
 * http://www.cs.cornell.edu/Courses/cs664/2008sp/handouts/cs664-2-filtering.pdf
 * if you would like to learn how Gaussian smoothing works. *)
let smooth (sigma : float) (img : image) : image =
	let mask = gaussianFilter sigma in
		convolveAndTranspose mask (convolveAndTranspose mask img)
