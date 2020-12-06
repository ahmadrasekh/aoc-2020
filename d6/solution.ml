#use "../lib/utils.ml"

module ChartSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = char
  end )

let parse_input =
  read_file >>
  group 

let group_to_set = List.map (
  explode >>
  ChartSet.of_list
)

let _ =
  "input" |>
  parse_input |>
  List.map group_to_set |>
  List.map (fun sets -> 
    List.fold_left 
      (fun s intersect -> ChartSet.inter s intersect) 
      (List.hd sets) 
      (List.tl sets)
  ) |>
  List.map ChartSet.cardinal |>
  list_add
