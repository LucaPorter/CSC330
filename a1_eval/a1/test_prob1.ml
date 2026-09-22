open Printf

let args = Array.to_list Sys.argv

let out =
  try
    match List.tl args with
    | ["is_older"; y1; m1; d1; y2; m2; d2] ->
        let r = Prob1.is_older
          ((int_of_string y1, int_of_string m1, int_of_string d1),
          (int_of_string y2, int_of_string m2, int_of_string d2))
        in
        sprintf "%b" r
    | ["days_in_month"; y; m] ->
        let r = Prob1.days_in_month (int_of_string y, int_of_string m) in
        (match r with Some i -> sprintf "%d" i | None -> "None")
    | ["dates_in_month"; y; m] ->
      let r = Prob1.dates_in_month (int_of_string y, int_of_string m) in
      (match r with
      | Some l ->
        let r = l |> List.map (fun (y, m, d) -> sprintf "%d %d %d" y m d) |> String.concat "," in
        sprintf "%s" r
      | None -> "None")
    | ["num_of_days"; y; m; d] ->
      let r = Prob1.num_of_days (int_of_string y, int_of_string m, int_of_string d) in
      (match r with Some i -> sprintf "%d" i | None -> "None")
    | ["nth_day"; y; n] ->
      let r = Prob1.nth_day (int_of_string y, int_of_string n) in
      (match r with Some (y, m, d) -> sprintf "%d %d %d" y m d | None -> "None")
    | _ -> sprintf "!ERR %s" (String.concat "; " args)
  with
  | Prob1.NotImplemented -> sprintf "!NIE %s" (List.hd @@ List.tl args)
  | e -> sprintf "!EXC %s" (Printexc.to_string e)
;;
flush_all ();
printf "[csc330_tester] %s\n" out
