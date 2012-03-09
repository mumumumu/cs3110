(* PROBLEM SET 2 - PART 2 *)
(* Name: Jeff Mu          *)
(* NetID: jm776           *)

type stack = int list
type memory = (string * int) list

(* An 'a cmd is something that expects a stack and produces an 'a
 * which can carry out the rest of the stack program. The actual
 * type 'a depends on what happens in the rest of the program.  *)
type 'a cmd = stack -> memory -> 'a
  
let start (c : 'a cmd) : 'a = c [] []

(* Note: halt is a stack cmd. *)
let halt (st : stack) (m : memory) = st
  
(* Pushes the integer n onto the top of the stack *)
let push (n : int) (st : stack) (m : memory) (c : 'a cmd) =
  c (n::st) m

(* Removes the top element of the stack *)
let pop (st : stack) (m : memory) (c : 'a cmd) = 
  match st with 
    | h::t -> c t m
    | [] -> raise (Failure "StackException")

(* Returns the value of the top element of the stack *)
let top (st : stack) = 
  match st with
    | h::t -> h
    | [] -> raise (Failure "StackException")

(* Pushes a duplicate copy of the top element onto the stack *)
let dup (st : stack) (m : memory) (c : 'a cmd) =
  push (top st) st m c
   
(* Pops the two top elements of the stack, computes their product, 
 * and pushes the result back onto the stack *) 
let mul (st : stack) (m : memory) (c : 'a cmd) =
  let a = top st in
  let b = top (pop st m halt) in
  let s = (pop (pop st m halt) m halt) in
    push (a * b) s m c

(* Pops the two top elements of the stack, computes their sum, 
 * and pushes the result back onto the stack *)
let add (st : stack) (m : memory) (c : 'a cmd) =
  let a = top st in
  let b = top (pop st m halt) in
  let s = (pop (pop st m halt) m halt) in
    push (a + b) s m c

(* Pops the top element of the stack n, then pops n more elements 
 * if n is positive. *)
let npop (st : stack) (m : memory) (c : 'a cmd) =
  let rec helper s n =
    if n > 0 then helper (pop s m halt) (n - 1)
    else c s m
  in helper (pop st m halt) (top st)

(* Stores the current value on top of the stack in memory under 
 * the string "str" *)
let store (str : string) (st : stack) (m : memory) (c : 'a cmd) =
  c st ((str, top st)::m)

(* Loads the value associated with the string "str" in memory
 * and puts it on top of the stack *)
let load (str : string) (st : stack) (m : memory) (c : 'a cmd) = 
  let n = List.fold_left (fun a h -> if (fst h) = str then snd h else a) 0 m
  in push n st m c

(* The following expression should evaluate to [11]
val test_val = start (push 2) (push 3) dup mul add halt;;
*)

