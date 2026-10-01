open Printf

let get_records contents =
  let rec to_float cols =
    match cols with
    | [] -> []
    | f :: cols -> Float.of_string_opt f :: to_float cols
  in
  let rec process rows =
    match rows with
    | [] -> []
    | row :: rows ->
      let cols = String.split_on_char ',' (String.trim row) in
      match cols with
      | name :: id :: rates -> Prob2.{ id; name; rates = to_float rates } :: process rows
      | _ -> failwith "cannot happen"
  in
  let rows = String.split_on_char '\n' contents in
  process rows

let read_file path =
  let fp = open_in path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s

let find rcs n = List.find (fun Prob2.{ id; _ } -> id = n) rcs

let args = Array.to_list Sys.argv

let out =
  try
    match List.tl args with
    | ["get_records"; path] ->
      let fc = read_file path in
      let rc = Prob2.get_records fc in
      String.concat ";" @@
        List.map
          (fun Prob2.{ id; name; rates } ->
            sprintf "%s,%s,%s" id name (
              List.map (fun x -> match x with Some f -> sprintf "%.1f" f | None -> "_") rates
            |> String.concat ","))
          rc
    | ["avail"; path; cty] ->
      let fc = read_file path in
      let rc = get_records fc in
      let cty = find rc cty in
      sprintf "%d" (Prob2.avail cty)
    | ["last"; path; cty] ->
      let fc = read_file path in
      let rc = get_records fc in
      let cty = find rc cty in
      sprintf "%s" (match Prob2.last cty with Some (y, r) -> sprintf "%d %.1f" y r | None -> "None")
    | ["minmax"; path; cty] ->
      let fc = read_file path in
      let rc = get_records fc in
      let cty = find rc cty in
      let mi, ma = Prob2.minmax cty in
      sprintf "%s %s"
        (match mi with Some (y, r) -> sprintf "%d %.1f" y r | None -> "None")
        (match ma with Some (y, r) -> sprintf "%d %.1f" y r | None -> "None")
    | ["summarize"; path; cty] ->
      let fc = read_file path in
      let rc = get_records fc in
      sprintf "%s" (Prob2.summarize (rc, cty))
    | "concat" :: sep :: lst ->
      let lst = List.map (fun x -> if x = "_" then "" else x) lst in
      sprintf "%s" (Prob2.concat (sep, lst))
    | _ -> sprintf "!ERR %s" (String.concat "; " args)
  with
  | Prob2.NotImplemented -> sprintf "!NIE %s" (List.hd @@ List.tl args)
  | e -> sprintf "!EXC %s" (Printexc.to_string e)
;;
flush_all ();
printf "[csc330_tester] %s\n" out
