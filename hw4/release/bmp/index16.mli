(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Fran�ois Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: index16.mli,v 1.1 2010/03/01 01:04:57 aowens Exp $*)

(** Indexed 16 bit depth image format *)

type elt = int;;

type rawimage;;

(* The image type *)
type t = {
  width : int;
  height : int;
  rawimage : rawimage;
  mutable infos : Info.info list;
  mutable colormap : Color.rgb Color.map;
  mutable transparent : int;
 };;

val to_rgb24 : ?failsafe: Color.rgb -> t -> Rgb24.t;;
val to_rgba32 : ?failsafe: Color.rgba -> t -> Rgba32.t;;
(** [to_rgb? ~failsafe t]: Image format conversion functions to Rgb24.t
   and Rgba32.t images. If the color for some pixel value is not defined,
   [failsafe] color is used as default. *)

(** Generic functions. *)
(** Please read the comments of IMAGEINDEXED in genimage.mli *)

val dump : t -> string;;
val unsafe_access : t -> int -> int -> string * int;;
val get_strip : t -> int -> int -> int -> string;;
val set_strip : t -> int -> int -> int -> string -> unit;;
val get_scanline : t -> int -> string;;
val set_scanline : t -> int -> string -> unit;;
val unsafe_get : t -> int -> int -> elt;;
val unsafe_set : t -> int -> int -> elt -> unit;;
val get : t -> int -> int -> elt;;
val set : t -> int -> int -> elt -> unit;;
val unsafe_get_color : t -> int -> int -> Color.rgb;;
val get_color : t -> int -> int -> Color.rgb;;
val unsafe_get_rgb : t -> int -> int -> Color.rgb;;
val get_rgb : t -> int -> int -> Color.rgb;;
val destroy : t -> unit;;
val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit;;
val map : (elt -> elt -> elt) ->
  t -> int -> int -> t -> int -> int -> int -> int -> unit;;
val create_with : int -> int ->
  Info.info list -> Color.rgb Color.map -> int -> string -> t;;
val create : int -> int -> t;;
val make : int -> int -> elt -> t;;
val copy : t -> t;;
val sub : t -> int -> int -> int -> int -> t;;
