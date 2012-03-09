type huffmantree = Empty | Node of node
and node = { freq : int; key : int; left : huffmantree; right : huffmantree }
    
type encoding = char * (int list)

(* Returns an array with the frequency of appearance of each character
   in cl stored in its ASCII-based index *)
let countchars (cl : char list) : int array =
  let ar = Array.make 256 0 in
   List.iter (fun elt -> ar.(int_of_char elt) <- (ar.(int_of_char elt) + 1)) cl;
   ar

(* Returns a list of all encodings stored in hm *)
let get_codes (hm : huffmantree) : encoding list =
  let rec helper t encl cw =
    match t with
      | Empty -> encl
      | Node {key = k; left = l; right = r} -> 
	  if k > 255 then
            List.rev_append (helper l encl (0::cw)) (helper r encl (1::cw))
          else [(char_of_int k, List.rev cw)]
  in helper hm [] []
  
(* Returns the longest encoding in encl*)
let longest_code (encl : encoding list) : int list =
  List.fold_left
    (fun l h ->
       if List.length (snd h) > List.length l then (snd h)
       else l)
    [] encl
    
(* Returns the bit vector in encl that encodes chr *)
let lookup_code (chr: char) (encl : encoding list) : int list =
  fst (List.fold_left 
         (fun (a,b) h -> 
            if (fst h) = chr then ((snd h),true)
	    else if b then (a,b)
	    else ([],false)) 
         ([],false) encl)
    
(* Returns the huffman tree created from cl and the encoded bit stream*)
let encode (cl : char list) : huffmantree * int list =
  let c = countchars cl in
  let leafs = List.rev 
    (snd (Array.fold_left 
            (fun (n, l) h ->
               (n+1, (Node {freq = h; key = n; left = Empty; right = Empty})::l)
            ) (0, []) c)) in 
  (* min2 uses list folding to find the two nodes with lowest frequencies *)          
  let min2 l =
    if List.length l < 2 then
      (Empty, Empty, l)
    else List.fold_left 
      (fun (n1, n2, rlst) x -> 
         match n1, n2, x with
           | Node {freq = f1}, Node {freq = f2}, Node {freq = f} ->
               if f1 > f then (x, n1, n2::rlst)
               else if f2 > f then (n1, x, n2::rlst)
               else (n1, n2, x::rlst) 
           | Empty,_,_ -> (x, n2, rlst)
           | _,Empty,_ -> (n1, x, rlst)
           | _,_,Empty -> (n1, n2, rlst)
      )
      (Empty,Empty,[]) l in
  (* build_tree recursively creates parent nodes for min2 nodes until only 1 *)
  let rec build_tree lst i =
    match List.tl lst with
      | [] -> lst
      | _ ->
          let (x, y, l) = min2 lst in
          let newnode = match (x, y) with
            | Node {key = k1; freq = f1}, Node {key = k2; freq = f2} ->
                  Node { freq = f1 + f2; key = i; left = y; right = x }
            | _,_ -> failwith "node list contains < 2 nodes"
          in build_tree (newnode::l) (i + 1) in 
  let ht = (List.hd (build_tree leafs 256)) in
  let el = get_codes ht in
  let enc = List.fold_left 
    (fun l h -> List.rev_append (lookup_code h el) l) 
    [] cl
  in (ht, List.rev enc)

(* Returns the char list decoded from bitstream, based on the bit vectors
   in hm *)
let decode (hm : huffmantree) (bitstream: int list) : char list =
  let rec helper ht bs cl =
    match bs, ht with
      | [], Node { key = k } -> cl
      | h::t, Node { key = k; left = l; right = r } -> 
          if r = Empty && l = Empty then helper hm bs ((char_of_int k)::cl)
          else if h = 0 then 
            (helper l t cl)
          else (helper r t cl)
      | _, Empty -> failwith "empty tree"
  in List.rev (helper hm bitstream [])

(* Returns bitstream with hm compressed and stored on the beginning of the
	 stream in preorder *)
let prepend_tree (hm : huffmantree) (bitstream: int list) : int list =
  let rec helper t lst = 
    match t with 
      | Empty -> lst
      | Node { key = k; left = l; right = r } -> 
         List.rev_append 
           (List.rev 
              (List.rev_append (List.rev (Util.to_bits 9 [] k)) (helper l lst)))
           (helper r lst)
  in (List.rev_append (List.rev (helper hm [])) bitstream)

(* Returns bitstream with the encoding for a huffman tree removed from the
   beginning and decoded.
   Requires: a valid encoding for a huffman tree at the beginning of
   bitstream *)
let regrow_tree (bitstream : int list) : huffmantree * int list =
  let (hm , enc, _) = 
    (* helper keeps track of how many nodes have been re-created *)
    let rec helper bs n = 
      let (k,s) = Util.read_n_bits_to_int 9 bs in
        if k > 255 && n < 510 then
          let (leftNode, s2, n2) = helper s (n + 1) in
          let (rightNode, s3, n3) = helper s2 (n2 + 1) in
            (Node {freq=0; key=k; left=leftNode; right=rightNode}, s3, n3)
        else
          (Node {freq=0;key=k;left=Empty;right=Empty}, s, n)
    in helper bitstream 0
  in (hm, enc)
       
let encode_file (fname : string) : unit =
  let (hm, codelist) = encode (Util.load_chars fname) in
  let prepended = prepend_tree hm codelist in
  let compressedf = open_out_bin (fname ^ "-compressed.txt") in
  let lc = longest_code (get_codes hm) in
  Util.write_bits compressedf prepended (lc);
  close_out compressedf

let decode_file (fname : string) : unit =
  let readcompf = open_in_bin (fname ^ "-compressed.txt") in
  let decodedf = open_out (fname ^ "-decoded.txt") in
  let readin = Util.read_bits readcompf in
  let (regrown, bitstream) = regrow_tree readin in
  let chrs = decode regrown bitstream in
  Util.write_chars decodedf chrs;
  close_in readcompf;
  close_out decodedf

let main (fname : string) : unit =
  encode_file fname; decode_file fname

let _ = main Sys.argv.(1)
