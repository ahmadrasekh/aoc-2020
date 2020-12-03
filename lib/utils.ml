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

(* ints  *)
let in_range (min, max) x =
  if min < max then min < x && x < max
  else failwith "range format: (min, max) with min < max"
