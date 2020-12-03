(* io *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* strings *)
let strip_last_char  = function
  | "" -> ""
  | _ as str -> String.sub str 0 ((String.length str) - 1)

let count_occurance ch str = 
    let char_seq = String.to_seq str in
    Seq.fold_left (fun acc c -> if c=ch then acc+1 else acc) 0 char_seq
    (* let rec aux acc = function
      | "" -> acc
      | hd::tl ->  *)

(* ints *)
let in_range (min, max) x =
  if min < max then min <= x && x <= max
  else failwith "range format: (min, max) with min < max"


(* logic *)
let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_of_int = function
  | 0 -> false
  | _ -> true



