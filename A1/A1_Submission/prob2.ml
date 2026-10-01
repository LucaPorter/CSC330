type country = {
  id : string;
  name : string;
  rates : float option list;
}


(* Pt.1, Problem 2: Parses file contents into a list of country records. 
 * Each line has the form: name,id,rate1,rate2,... ; 
 *   blank lines and malformed lines are skipped.
 *)
let get_records (contents : string) : country list =
  let lines = String.split_on_char '\n' contents in
  (* Parses each rate field into Some x, or None if the field is invalid or empty *)
  let rec map_rates (fields : string list) : float option list =
    match fields with
    | [] -> []
    | r :: tail -> Float.of_string_opt (String.trim r) :: map_rates tail
  in
  let rec process_info (lines : string list) : country list =
    match lines with
    | [] -> []
    | line :: rest ->
      let trimmed = String.trim line in
      if trimmed = "" then process_info rest
      else
        match String.split_on_char ',' line with
        | name :: id :: rate_entries ->
          let rates = map_rates rate_entries in
          { id = String.trim id; name = String.trim name; rates = rates } :: process_info rest
        | _ -> process_info rest
  in
  process_info lines;;

(* Pt.2, Problem 2: Counts how many years in c.rates have an actual (Some) value
 *  returns: int
 *)
let avail (c : country) : int =
  let rec count_available (rates : float option list) (count : int) : int =
    match rates with
    | [] -> count
    | Some _ :: tail -> count_available tail (count + 1)
    | None :: tail -> count_available tail count
  in
  count_available c.rates 0;;

(* Pt.3, Problem 2: Finds the most recent year with an available rate
 *   Rates are supposed to start at year 1960 
 *   returns: (int * float) option
 *)
let last (c : country) : (int * float) option =
  let rec get_last (rates : float option list) (year : int) (best : (int * float) option) : (int * float) option =
    match rates with
    | [] -> best
    | Some r :: tail -> get_last tail (year + 1) (Some (year, r))
    | None :: tail -> get_last tail (year + 1) best
  in
  get_last c.rates 1960 None;;

(* Pt.4, Problem 2: Finds the (year, rate) pairs with the lowest and highest rates
 *   returns (int * int) option * (int * int) option
 *)
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


(* Looks up a country by id; returns None if no match is found. *)
let rec find_country (countries : country list) (id : string) : country option =
  match countries with
  | [] -> None
  | c :: tail -> if c.id = id then Some c else find_country tail id;;


(* Pt.5, Problem 2: Creates a report of a country's inflation data 
 *   returns: string
 *)
let summarize ((countries : country list), (id : string)) : string =
  match find_country countries id with
  | None -> "Cannot Find " ^ id
  | Some c ->
    let n = avail c in
    let last_rec =
      match last c with
      | Some (year, rate) -> "Last record: " ^ string_of_int year ^ " with rate of " ^ string_of_float rate ^ "%"
      | None -> ""
    in
    let (min, max) = minmax c in
    let lowest_rate =
      match min with
      | Some (year, rate) -> "Lowest rate: " ^ string_of_int year ^ " with rate of " ^ string_of_float rate ^ "%"
      | None -> ""
    in
    let highest_rate =
      match max with
      | Some (year, rate) -> "Highest rate: " ^ string_of_int year ^ " with rate of " ^ string_of_float rate ^ "%"
      | None -> ""
    in
    "Country: " ^ c.name ^ " (" ^ c.id ^ ")\nRecords available: " ^ string_of_int n ^ " years\n"
    ^ last_rec ^ "\n" ^ lowest_rate ^ "\n" ^ highest_rate ^ "\n";;


(* Pt.6, Problem 2: Joins strings with separator, skipping any empty strings entirely. 
 *  returns: string
 *)
let concat ((separator : string), (strings : string list)) : string =
  let rec concatenator (lst : string list) (cur_str : string) : string =
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



