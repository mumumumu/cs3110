
(* Returns a char list of the contents of the file at fname *)
let load_chars (fname : string) : char list =
  let explode (s : string) : char list =
    let rec addtolist n acc =
      if n < 0 then acc else
        addtolist (n - 1) (s.[n] :: acc) in
    addtolist (String.length s - 1) ['\n'] in
  let file = open_in fname in
  let stringlist =
    try
      let rec read_lines acc =
        let next = try Some (input_line file) with End_of_file -> None in
        match next with Some s -> read_lines (s :: acc) | None -> acc in
      read_lines []
    with exc -> close_in file; raise exc in
  List.concat (List.rev_map explode stringlist)

let to_bits (n : int) (bitstream : int list) (num : int) : int list =
  let rec each_bit accum i =
    if i < n then
      each_bit (((num lsr i) land 1) :: accum) (i + 1)
    else accum in
  each_bit bitstream 0

(* Returns bitstream with the first n bits removed and converted back
 * to an integer *)
let read_n_bits_to_int (n : int) (bitstream : int list) : int * int list =
  let rec pop_n i word lst =
    if i = n then (word, lst)
    else match lst with
        h :: t -> pop_n (i + 1) ((word lsl 1) lor h) t
      | [] -> failwith "not enough bits remaining"
  in pop_n 0 0 bitstream

(* Write a stream of bits to a byte-oriented out channel.*)
(* Pad last byte to length 8 with the supplied padding. *)
let write_bits (f : out_channel) (bits : int list) (padding : int list) : unit =
  assert (List.length padding >= 8); (* sanity check *)
  let next_byte bits =
    let rec nb n byte bits =
      if n = 8 then (byte, bits)
      else match bits with
        | [] -> (fst (nb n byte padding), [])
        | h :: t -> nb (n + 1) (byte lsl 1 lor h) t
    in nb 0 0 bits in
  let rec write_char bits =
    if bits = [] then () else
    let (byte, bits) = next_byte bits in
    output_byte f byte; write_char bits in
  write_char bits;
  close_out f

let write_chars (f : out_channel) (chrs : char list) : unit =
  List.iter (output_char f) chrs

(* Returns the contents of f as a bitstream *)
let read_bits (f : in_channel) : int list =
  (* reversed list of bytes *)
  let rec read_bytes bytes =
    let res = try Some (input_byte f)
      with End_of_file -> None in
    match res with
      None -> bytes
    | Some(b) -> read_bytes (b :: bytes) in
  List.fold_left (to_bits 8) [] (read_bytes [])
