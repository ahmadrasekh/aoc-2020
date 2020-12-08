#use "../lib/utils.ml"


let parse_input f =
  let lines = read_file f in
  let visited = Array.make (List.length lines) false
  and tape = 
    lines |> 
    List.map (fun s -> Scanf.sscanf s "%3s %s" (fun cmd op -> (cmd, (int_of_string op)))) |>
    Array.of_list  in
  (* and cmds = Array.make (List.length lines) "nop" *)
  (* and ops = Array.make (List.length lines) 0 in *)
  (* List.iteri (fun i s -> 
    Scanf.sscanf s "%3s %s" (fun cmd op -> 
      cmds.(i) <- cmd;
      ops.(i) <- (int_of_string op)
    )
  ) lines; *)
  (visited, tape)

let run (visited, tape) =
  let rec aux i acc =
    if visited.(i) then acc else begin
      visited.(i) <- true; 
      let (cmd, op) = tape.(i) in
      match cmd with 
        | "nop" -> aux (i+1) acc
        | "acc" -> aux (i+1) (acc+op)
        | "jmp" -> aux (i+op) acc
        | _ -> failwith "unrecognised command!"
    end in
  aux 0 0

let _ =
  "input" |>
  parse_input |>
  run
  (* (fun (_, tape) -> List.filter (fun op -> op="jmp") (Array.to_list cmds)) |>
  List.length *)