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

(* other *)
let (>>) f g x = g(f(x));;

(* lists *)
let count_in_list x ls = 
  List.fold_left (fun acc elem -> if elem=x then acc+1 else acc) 0 ls

let rec split_at n acc l =
  if n = 0 then 
    (List.rev acc, l) 
  else
    match l with
      | [] -> (List.rev acc, [])
      | h :: t -> split_at (n-1) (h :: acc) t

(* strings *)
let strip_last_char  = function
  | "" -> ""
  | _ as str -> String.sub str 0 ((String.length str) - 1)

let count_in_string ch = 
  String.to_seq >>
  List.of_seq >>
  count_in_list ch

(* ints *)
let in_range (min, max) x =
  if min < max then min <= x && x <= max
  else failwith "range format: (min, max) with min < max"

let list_multiply = List.fold_left (fun acc x -> acc * x) 1

(* logic *)
let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_of_int = function
  | 0 -> false
  | _ -> true



