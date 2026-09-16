type country = {
  id : string;
  name : string;
  rates : float option list;
}
let get_records (contents : string) : country list = 
  let lines = String.split_on_char '\n' contents in 
  let rec process_info (lines : string list) : country list =
    match lines with
    | [] -> []
    | line :: rest ->
      let trimmed = String.trim line in
      if trimmed = "" then process_info rest
      else 
        match String.split_on_char ',' line with 
        | name :: id :: rate_entries ->
          let rates = List.map (fun r -> Float.of_string_opt (String.trim r)) rate_entries in
          { id = String.trim id; name = String.trim name; rates = rates} :: process_info rest
        | _ -> process_info rest
        
  in
  process_info lines;; 

let read_file path =
  let fp = open_in path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s

let print_country (c : country) : unit =
  Printf.printf "id=%s name=%s rates=[" c.id c.name;
  let rec print_rates rs =
    match rs with
    | [] -> ()
    | Some r :: rest ->
        Printf.printf "%f; " r;
        print_rates rest
    | None :: rest ->
        Printf.printf "None; ";
        print_rates rest
  in
  print_rates c.rates;
  Printf.printf "]\n" ;;

let () =
  let contents = read_file "csc330_a1.csv" in
  let records = get_records contents in
  Printf.printf "Number of records: %d\n" (List.length records);
  List.iter print_country records 

  (*let () =
  let contents = read_file "csc330_a1.csv" in
  Printf.printf "%s\n" contents*)