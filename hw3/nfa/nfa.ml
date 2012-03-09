(* PROBLEM SET 3 - PART 3 *)
(* Name: Jeff Mu          *)
(* NetID: jm776           *)

#use "regexp_util.ml";;

module Util = struct
  open List

  (* List of contiguous integers: 1 <|> 5 = [1; 2; 3; 4; 5] *)
  let (<|>) x y =
    let rec loop i acc = if i > y then rev acc else loop (i+1) (i::acc) in
    loop x []

  let range n = 0 <|> n-1

  let (^^) f = fun l -> concat (map f l)

  (* Takes a string and returns a list of characters *)
  let explode str =
    let rec expl i l = if i < 0 then l else expl (i-1) (str.[i] :: l) in
    expl (String.length str - 1) []
  let implode s = String.concat "" (List.map (fun c -> String.make 1 c) s)

  let alphabet = explode "abcdefghijklmnopqrstuvwxyz"

  (* Removes duplicates from a list (useful for epsilon_closure) *)
  let unique l =
    let uniq e = function
      | [] -> [e]
      | h::_ as l -> if e = h then l else e :: l in
    fold_right uniq (sort compare l) []
end

(* Immutable 2D tables *)
module Table
  : sig
    type 'a t
    (* Vertical dimension, horizontal dimension, default value *)
    val create : int -> int -> 'a -> 'a t

    (* Table, row index, column index, new value *)
    val add : 'a t -> int -> int -> 'a -> 'a t
    val lookup : 'a t -> int -> int -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t

    (* Concatenates the rows of two tables *)
    val concat : 'a t -> 'a t -> 'a t

    (* Adds an element to a table of lists *)
    val append : 'a list t -> int -> int -> 'a -> 'a list t

    val height : 'a t -> int
    val width : 'a t -> int
  end

  = struct
    type 'a t = 'a array array

    let create m n v =
      let rows = List.map (fun _ -> Array.make n v) (Util.range m) in
      Array.of_list rows

    let add t m n v =
      let new_row = Array.copy t.(m) in
      let new_t   = Array.copy t in
      new_row.(n) <- v;
      new_t.(m) <- new_row;
      new_t

    let lookup t m n = try t.(m).(n) with _ -> Printf.printf "%d %d\n" m n; 
			t.(m).(n)
    let map f t = Array.map (Array.map f) t
    let concat = Array.append
    let append t m n v =
      let old = lookup t m n in
      add t m n (v :: old)

    let height t = Array.length t
    let width t = Array.length t.(0)
  end

module NFA
  : sig
    type state = int
    type nfa
    type transition = state * char option * state

    (* Assumes state list is [0..n_states-1] *)
    val make :
      start:state list ->
      accept:state list ->
      n_states:int ->
      alphabet:char list ->
      transitions:transition list -> nfa
    val count_states : nfa -> int
    val lookup : nfa -> state -> char option -> state list
    val accept : nfa -> string -> bool
	
    (* All operations are done immutably *)
    val union : nfa -> nfa -> nfa
    val concat : nfa -> nfa -> nfa
    val star : nfa -> nfa
  end

  = struct
    open Util
    type state = int
    type transition = state * char option * state

    module StateSet = Set.Make
      (struct
        type t = state
        let compare = compare
       end)

    module Dict = Map.Make
      (struct
        type t = char
        let compare = compare
       end)

    type nfa = {table : state list Table.t;
                start : state list;
                final : StateSet.t;
                alphabet : int Dict.t}

    (* Epsilon (as None) always maps to zero *)
    let char_to_int alphabet = function
      | None -> 0
      | Some c -> Dict.find c alphabet

    let add_trans alphabet table (s0, c, s1) =
      let n = char_to_int alphabet c in
      Table.append table s0 n s1
    let lookup nfa state c =
      Table.lookup (nfa.table) state (char_to_int nfa.alphabet c)

    (* Assumes states are integers in [0..n_states-1] *)
    let make ~start ~accept ~n_states ~alphabet ~transitions =
	  	let (abc,len_abc) = (List.fold_left (fun (d,i) a -> (Dict.add a i d,i+1))
    		(Dict.empty, 1) alphabet) in
	  	let t_table_init = Table.create n_states len_abc [] in
	  	let t_table = List.fold_left 
	   		(fun t h -> add_trans abc t h) t_table_init transitions in
    	let f_states = List.fold_left (fun s h -> StateSet.add h s)
				StateSet.empty accept in
	  	{table = t_table; start = start; final = f_states; alphabet = abc}

    let count_states nfa =
      Table.height (nfa.table)

    let epsilon_closure nfa states =
      (* helper keeps track of all epsilon-move states *)
      let rec helper current_states end_states =
        match current_states with
          | [] -> end_states
          | _ -> let next_states = List.fold_left 
            (fun t h -> (List.rev_append (Table.lookup nfa.table h 0) t)) 
            [] current_states in
            let n = List.rev_append next_states end_states in
            helper next_states n
      in helper states states 

    (* Precondition: current_states is epsilon_closed *)
    let step nfa current_states char =
	  	let i = Dict.find char nfa.alphabet in
	  	let next_states = List.fold_left
				(fun s h -> List.rev_append s (Table.lookup nfa.table h i))
        [] current_states in
	  	epsilon_closure nfa next_states

    let accept nfa string =
	  	let s = explode string in
	  	let final_states = List.fold_left (fun t h -> step nfa t h) 
        (epsilon_closure nfa (nfa.start)) s in
      List.fold_left (fun b h -> b || StateSet.mem h nfa.final) 
        false final_states

    (* Assumes alphabets are the same *)
    let union nfa1 nfa2 =
      let plus_n = (List.map ((+) (count_states nfa1))) in
      let new_start = List.rev_append nfa1.start (plus_n nfa2.start) in
      let new_final = StateSet.union nfa1.final 
        (List.fold_left (fun s h -> StateSet.add h s) 
        StateSet.empty (plus_n (StateSet.elements nfa2.final))) in
      let new_table = Table.concat nfa1.table (Table.map plus_n nfa2.table) in
      {table = new_table; start = new_start; final = new_final; 
        alphabet = nfa1.alphabet}
	  
    (* Adds epsilon transitions from accepts of nfa1 to starts of nfa2 *)
    let concat nfa1 nfa2 =
      let plus_n = (List.map ((+) (count_states nfa1))) in
	    let new_start = nfa1.start in
	    let new_final = List.fold_left (fun s h -> StateSet.add h s) 
        StateSet.empty (plus_n (StateSet.elements nfa2.final)) in
      let transitions = List.fold_left 
        (fun t s0 -> List.rev_append t 
          (List.fold_left (fun t s1 -> (s0,None,s1)::t) [] (plus_n nfa2.start))) 
          [] (StateSet.elements nfa1.final) in
	    let new_table_init = Table.concat nfa1.table 
        (Table.map plus_n nfa2.table) in
	    let new_table = List.fold_left 
	   		(fun t h -> add_trans nfa1.alphabet t h) new_table_init transitions in
      {table = new_table; start = new_start; final = new_final; 
        alphabet = nfa1.alphabet}

    (* Makes every start state a final state and adds epsilon transitions *)
    (* from every accept state to every start state *)
    let star nfa =
      let new_final = StateSet.union nfa.final (List.fold_left 
        (fun s h -> StateSet.add h s) StateSet.empty nfa.start) in
      let transitions = List.fold_left 
        (fun t s0 -> List.rev_append t 
          (List.fold_left (fun t s1 -> (s0,None,s1)::t) [] nfa.start)) 
          [] (StateSet.elements nfa.final) in
      let new_table = List.fold_left
	   		(fun t h -> add_trans nfa.alphabet t h) nfa.table transitions in
      {table = new_table; start = nfa.start; final = new_final; 
        alphabet = nfa.alphabet}
  end

module Levenshtein_Automata
  = struct
    open Util

    (* Accepts strings of edit distance strictly less than k_max *)
    let make string k_max =
      let chars = List.map (fun h -> Some(h)) (explode string) in 
      let n_chars = List.length chars in
      let n_states = (k_max + 1) * (n_chars + 1) in
      let start = [0] in
      let transitions =
        let star s1 s2 = List.fold_left (fun s h -> (s1,Some(h),s2)::s)
 				  [] alphabet in
        let epsilon s1 s2 = [(s1,None,s2)] in
        (* build the transition list based on size of the string and # nodes*)
        let rec build tr i a =
          match a with
            | [] -> if i < (n_states-1) then build ((star i (i+1))@tr) (i+1) a
									  else tr
            | h::t -> if i mod (k_max+1) = k_max then 
												build ((i,h,i+k_max+1)::tr) (i+1) t
                      else build ([(i,h,i+k_max+1)]@(star i (i+1))@(star i
											  (i+k_max+2))@(epsilon i (i+k_max+2))@tr) (i+1) a 
        in build [] 0 chars in
      let final_states = 
        (* creates a list containt the last k nodes *)
        let rec helper k n f =
          if k = 0 then f 
          else helper (k-1) (n-1) (n::f)
        in helper (k_max+1) (n_states-1) [] in
      NFA.make start final_states n_states alphabet transitions
      
  end

module Regexp
  : sig
    type regexp
    val build : string -> regexp
    val accept : regexp -> string -> bool
  end
  = struct
    open Util
    open RegexpUtil
    type regexp = NFA.nfa

    let build str =
      let rec helper r =
        match r with
					| Epsilon -> NFA.make [0] [1] 2 alphabet [(0,None,1)]
          | Char (a) -> NFA.make [0] [1] 2 alphabet [(0,Some(a),1)]
					| Union (a,b) -> NFA.union (helper a) (helper b)
					| Concat (a,b) -> NFA.concat (helper a) (helper b)
					| Star (a) -> NFA.star (helper a)
					| Fuzzy (a,b) -> Levenshtein_Automata.make (implode a) b 
      in helper (parse str)
    let accept nfa str = NFA.accept nfa str
  end
