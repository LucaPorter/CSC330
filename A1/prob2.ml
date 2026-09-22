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

let avail (c : country) : int = 
  let rec count_available (rates : float option list) (count : int) : int = 
    match rates with
    | [] -> count
    | Some _ :: tail -> count_available tail (count + 1)
    | None :: tail -> count_available tail count
  in
  count_available c.rates 0;;


let last (c : country) : (int * float) option = 
  let rec get_last (rates: float option list) (year : int) (best : (int * float) option): (int * float) option = 
    match rates with 
    | [] -> best
    | Some r :: tail -> get_last tail (year + 1) (Some (year,  r))
    | None :: tail -> get_last tail (year + 1) best
  in
  get_last c.rates 1960 None;;
  

let minmax (c : country) : (int * float) option * (int * float) option = 
  let rec find_minmax 
      (rates : float option list)
      (year : int)
      (choice : (int * float) option * (int * float) option)
      : (int * float) option * (int * float) option =
    match rates with
    | [] -> choice
    | None :: tail -> find_minmax tail (year + 1) choice
    | Some r :: tail -> 
      let (min, max) = choice in
      let new_min =
        match min with
        | None -> Some (year, r)
        | Some (_, m) -> if r < m then Some (year, r) else min
      in 
      let new_max =
        match max with
        | None -> Some (year, r)
        | Some (_, m) -> if r > m then Some (year, r) else max
      in
      find_minmax tail (year + 1) (new_min, new_max)
  in find_minmax c.rates 1960 (None, None);;

let summarize ((countries : country list), (id : string)) : string =
  match List.find_opt (fun c -> c.id = id) countries with
  | None -> Printf.sprintf "Cannot Find %s" id
  | Some c -> 
    let n = avail c in
    let last_rec = 
      match last c with
      | Some (year, rate) -> Printf.sprintf "Last Record: %d with rate of %.1f%%" year rate
      | None -> "Lowest Rate: None"
    in
    let (min, max) = minmax c in
    let lowest_rate =
      match min with
      | Some (year, rate) -> Printf.sprintf "Lowest Rate: %d with rate of %.1f%%" year rate
      | None -> "Lowest Rate: None"
    in
    let highest_rate =
      match max with 
      | Some (year, rate) -> Printf.sprintf "Highest Rate: %d with rate of %.1f%%" year rate
      | None -> "Highest  Rate: None"
    in
    Printf.sprintf "Country: %s (%s)\nRecords Available: %d years\n%s\n%s\n%s\n" 
     c.name c.id n last_rec lowest_rate highest_rate;;

let concat ((separator : string), (strings : string list)) : string = 
  let rec concatenator (lst: string list) (cur_str : string) : string =
    match lst with 
    | [] -> cur_str
    | s :: tail -> 
      if s = "" then concatenator tail cur_str
      else if cur_str = "" then concatenator tail s
      else concatenator tail (cur_str ^ separator ^ s)
    in 
    concatenator strings ""
      

let read_file path =
  let fp = open_in path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s

let show (label : string) (result : string) : unit =
  Printf.printf "%-30s -> \"%s\"\n" label result

let test_concat () =
  show "concat (\", \", [\"a\"; \"\"; \"b\"])" (concat (", ", ["a"; ""; "b"]));
  show "concat (\", \", [])" (concat (", ", []));
  show "concat (\", \", [\"\"])" (concat (", ", [""]));
  show "concat (\", \", [\"\"; \"\"; \"\"])" (concat (", ", [""; ""; ""]));
  show "concat (\", \", [\"a\"])" (concat (", ", ["a"]));
  show "concat (\", \", [\"\"; \"a\"])" (concat (", ", [""; "a"]));
  show "concat (\", \", [\"a\"; \"\"])" (concat (", ", ["a"; ""]));
  show "concat (\", \", [\"a\";\"b\";\"c\"])" (concat (", ", ["a"; "b"; "c"]));
  show "concat (\"-\", [\"a\"; \"b\"])" (concat ("-", ["a"; "b"]));
  show "concat (\"\", [\"a\"; \"b\"])" (concat ("", ["a"; "b"]));
  show "concat (\", \", [\"a\";\"b\";\"c\";\"d\"])" (concat (", , , ", ["a"; "b"; "c"; "d"]))

let () = test_concat ();;


(*
let () =
  let contents = read_file "csc330_a1.csv" in
  let records = get_records contents in
  print_endline (summarize (records, "ABW"));
  print_endline (summarize (records, "CAN"));
  print_endline (summarize (records, "XXXNOPE"));;

let test_minmax (country_name : string) =
  let contents = read_file "csc330_a1.csv" in
  let records = get_records contents in
  let c = List.find (fun c -> c.name = country_name) records in
  match minmax c with
  | (Some (min_y, min_r), Some (max_y, max_r)) ->
    Printf.printf "%s: min = (%d, %f), max = (%d, %f)\n" c.name min_y min_r max_y max_r
  | _ -> Printf.printf "%s: no data\n" c.name

let () = test_minmax "Aruba"
let () = test_minmax "Canada";;


let test_last (country_name : string) =
  let contents = read_file "csc330_a1.csv" in
  let records = get_records contents in
  let c = List.find (fun c -> c.name = country_name) records in
  match last c with
  | Some (year, rate) -> Printf.printf "last %s = (%d, %f)\n" c.name year rate
  | None -> Printf.printf "last %s = None\n" c.name

let () = test_last "Aruba"
let () = test_last "Canada"
let () = test_last "United States";;
  

let test_avail (country_name : string) =
  let contents = read_file "csc330_a1.csv" in
  let records = get_records contents in
  let c = List.find (fun c -> c.name = country_name) records in
  Printf.printf "avail %s = %d\n" c.name (avail c);
  Printf.printf "total rates for %s = %d\n" c.name (List.length c.rates)

let () = test_avail "Aruba"
let () = test_avail "Canada"


  
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
*)

  (*let () =
  let contents = read_file "csc330_a1.csv" in
  Printf.printf "%s\n" contents*)