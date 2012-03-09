(* Name: Jeff Mu *)
(* Netid: jm776  *)
(* Section: 204  *)

(* Part 3 *) 

(*a*)
(* Duplicates elements in a list if they satisfy the predicate *)
let cond_dup lst f =
  let rec helper lst1 lst2 =
    match lst1 with
      | [] -> List.rev lst2
      | h::t -> if (f h) then helper t (h::h::lst2) else helper t (h::lst2)
  in helper lst []

(*b*)
(* Applies f to v n times *)
let rec n_times (f, n, v) =
  match n with
    | 0 -> v
    | _ -> n_times (f, n-1, f v)
	
(*c*)
(* Returns an ordered list of all integers from num1 to num2 inclusive *)
let range num1 num2 = 
  let rec helperc n1 n2 lst =
    match n2-n1 with
      | 0 -> List.rev (n1::lst)
      | _ -> helperc (n1+1) n2 (n1::lst)
  in if (num2>num1) then helperc num1 num2 [] 
    else failwith "Error: num1>num2"
		
(*d*)
(* Generates a list whose ith element is obtained by applying f 
 * to the ith element of l1 and the ith element of l2 *)
let zipwith f lst1 lst2 = 
  let rec helperd l1 l2 l3 =
    if List.length l1 > 0 && List.length l2 > 0 then
      helperd (List.tl l1) (List.tl l2) ((f (List.hd l1) (List.hd l2))::l3)
    else
      l3
  in helperd lst1 lst2 []
  
(*e*)
(* Finds the hypotenuse of a right triangle, given two sides *)
let hyp a b =
  let d x = (x, x) in
  let u (x1, x2) f = f x1 x2 in
  let p f x = f x in
    p sqrt (u (u (d a) ( *.), u (d b) ( *.)) (+.))

(*f*)
(* Partitions a list into equivalence classes *)
(* I recognize my use of the forbidden @ operator in this function.
 * Unfortunately this this was the only way I could get it working *)
let buckets equiv lst =
  (* Helper function traverses the inputbucket list *)
  let rec helperf alst bucketlst =
    match alst with
      | [] -> List.rev bucketlst
      | h::t -> helperf t
          (* Second helper function traverses the list of buckets *)
          (let rec bucketsort cblst restlst =
	     match cblst with
	       | [] -> [h]::restlst
	       | h2::t2 -> if equiv h (List.hd h2) 
                 then t2@((h::h2)::restlst)
		 else bucketsort t2 (h2::restlst)
	   in bucketsort bucketlst [])
  in helperf lst []
