#use "../lib/utils.ml"

let input = "input" |> read_file |> List.map int_of_string

let find_solution ~sum:total ~input:input ~sum_of:sm= 
  let h = Hashtbl.create (List.length input) in

  let rec sum_of_two sum = function 
    | hd::tl ->  let need = sum-hd in 
                  if Hashtbl.mem h (need) then
                    [hd; need]
                  else begin
                    Hashtbl.add h hd hd;
                    sum_of_two sum tl
                  end
    | [] -> [] in

  let rec sum_of_three sum = function
    | hd::tl -> (
                  match sum_of_two (sum-hd) tl with
                    | [] -> sum_of_three sum tl
                    | _ as sum_of_two_solution -> hd::sum_of_two_solution
                )
    | [] -> [] in

    let solution = match sm with 
      | 2 -> sum_of_two total input
      | 3 -> sum_of_three total input
      | _ -> [] in
    
    (* assert ((List.fold_left (+) 0 solution) = total); *)
    
    solution

