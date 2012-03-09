(* PROBLEM SET 2 - PART 1 *)
(* Name: Jeff Mu          *)
(* NetID: jm776           *)

(* Returns the second-smallest element of a list *)
let min2 (lst : int list) : int =
  if List.length lst < 2 then
    raise (Failure "List contains less than 2 elements")
  else snd (
    List.fold_left 
      (fun (y1,y2) x ->
         if y1>x then (x,y1)
         else if y2>x then (y1,x)
         else (y1,y2)) 
      (max_int,max_int) lst
  )

(* Removes consecutive duplicate values from a list *)
let consec_dedupe (equiv:'a -> 'a -> bool) (lst:'a list): 'a list =
  List.rev (
    snd (List.fold_left 
           (fun (y,l) x -> 
	      if equiv x y then (x,l) else (x,x::l))
           (max_int,[]) lst)
  )

(* Returns a list of all non-empty prefixes of a list, 
 * ordered from shortest to longest *)
let prefixes (lst: 'a list): 'a list list =
  List.rev (
    snd (List.fold_left
           (fun (pfx,plst) x ->
              (x::pfx,(List.rev (x::pfx))::plst))
           ([],[]) lst)
  )
   
(* Given a list of integers lst and an integer k, compute the contiguous 
 * sublist of length k whose elements have the largest sum. *) 
let k_sublist (lst : int list) (k : int) : int list =
  if List.length lst < k || k < 0 then
    raise (Failure "List does not contain at least k elements")
  else List.rev (fst
    (* Fold across the list, keep track of the sublist with the largest sum *) 
    (List.fold_left
       (fun (lst1,lst2) x ->
          let l1,l2 = List.length lst1, List.length lst2 in
            if l1 < k && l2 < k then (x::lst1,x::lst2)
            (* compare the current sublist to the current largest sum sublist *)
            else let cur_lst = x::(List.rev (List.tl (List.rev lst2))) in
              (if (List.fold_left (+) 0 lst1) >= (List.fold_left (+) 0 cur_lst)
               then (lst1,cur_lst) 
               else (cur_lst,cur_lst)))
       ([],[]) lst))

(* Returns the powerset of lst *)    
let powerset (lst : 'a list) : ('a list list) =
  List.fold_left 
    (fun pset x -> pset@(List.map (List.append [x]) pset)) 
    [[]] lst
    

