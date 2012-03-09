type element = int

(* represents a universe of elements U = {0, 1, 2, ..., m} *)
type universe

(* create a universe consisting of the elements {0, 1, 2, ..., n} *)
val createUniverse : int -> universe

(* union u e1 e2 joins the sets containing e1 and e2
 * Requires: e1, e2 are in u *)
val union : universe -> element -> element -> unit

(* find u e returns the representative of the set containing e
 * Requires: e is in u *)
val find : universe -> element -> element

(* size u e is the number of elements
 * in the set represented by e. Requires: e is in u *)
val size : universe -> element -> int

