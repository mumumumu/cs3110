(* PROBLEM SET 2 - PART 3 *)
(* Name: Jeff Mu          *)
(* NetID: jm776           *)

#load "str.cma";;

(* Takes in a document as a string list, and a term as a string, 
 * and returns the log-weighted Term Frequency  *)
let tf (lst : string list) (word : string) : float =
  let freq = 
    List.fold_left (fun c t -> if word = t then c +. 1.0 else c) 0.0 lst
  in if freq > 0.0 then 1.0 +. log10 freq else 0.0

(* Takes in the database (list of documents) and a term 
 * and returns the log-weighted Inverse Document Frequency*)
let idf (lst: string list list) (word:string):float = 
  let df = 
    List.fold_left (fun f d -> if List.mem word d then f +. 1.0 else f) 0.0 lst
  in if df > 0.0 then log10 ((float_of_int (List.length lst)) /. df)
    else raise (Failure "df = 0")

(* Takes a database of documents, represented as a list of pairs of integers and * strings; a search query, represented as a list of strings; 
 * and returns the identifier of the document with the highest similarity, as 
 * well as its similarity*)
let doc_rank (database : (int * string list) list) (input:string list)
    : int*float = 
  (* First fold iterates across all documents looking for highest scoring doc *)
  List.fold_left 
    (fun (id,score) doc -> 
       let new_score =
         (* Second fold iterates across words in the input to calculate score *)
         (List.fold_left 
            (fun tfidf word -> 
               tfidf +. ((tf (snd doc) word)
                         *. (idf (snd (List.split database)) word)))
            0.0 input)
       in if new_score > score then (fst doc,new_score) else (id,score))
    (0,0.0) database
    
let load_documents (filename : string) : (int*string list) list =
  let f = open_in filename in
  let rec next accum =
    match (try Some (input_line f) with End_of_file -> None) with
    | None -> accum
    | Some line ->
      (match Str.split (Str.regexp "@\\|$") line with
        | [id; contents] ->
          next ((int_of_string id, Str.split 
		    (Str.regexp "[ \t]+") contents) :: accum)
        | _ -> failwith "malformed input") in
  let docs = next [] in
  close_in f;
  docs
