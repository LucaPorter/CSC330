(*
Part 1 of problem 1: Takes 2 dates and returns true if the firt is earlier than the second
  returns: bool
*)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) : bool =
  let (a1, b1, c1) = date1 in
  let (a2, b2, c2) = date2 in
  if a1 <> a2 then a1 < a2
  else if b1 <> b2 then b1 < b2 
  else c1 < c2 ;;

(*
Part 2 of problem 1: Takes a year and month as input and returns the number of days in the month
  returns: int option
*)
let days_in_month ((a : int), (b : int)) : int option =
  let is_leap_year = (a mod 4 = 0 && a mod 100 <> 0) || a mod 400 = 0 in
  if b = 1 || b = 3 || b = 5 || b = 7 || b = 8 || b = 10 || b = 12 then Some 31
    else if b = 4 || b = 6 || b = 9 || b = 11 then Some 30
    else if b = 2 then (if is_leap_year then Some 29 else Some 28)
    else None;;

  (* 
  Part 3 of problem 1: Takes a year and month and finds all possible dates in that month
    returns: (int * int * int) list option 
  *)
let dates_in_month ((a : int), (b : int)) : (int * int * int) list option = 
  match days_in_month (a, b) with
  | None -> None
  | Some d -> 
      let rec build_string (x : int) : (int * int * int) list = 
        if x > d then []
        else (a, b, x) :: build_string (x + 1)
      in
      Some (build_string 1);;

(*
  Part 4 of Problem 1: 
*)
let num_of_days ((a : int), (b : int), (c : int)) : int option = 
  let rec sum_before_current_month (year : int) (month : int) (current_month : int) : int =
    if month >= current_month then 0
    else match days_in_month (year, month) with
      | None -> 0
      | Some d -> d + sum_before_current_month year (month + 1) current_month
  in
  match days_in_month(a, b) with
  | None -> None
  | Some _ -> Some (sum_before_current_month a 1 b + c);;

(*
  Part 5 of Problem 1: 
*)
let nth_day ((year : int), (n : int)) : (int * int * int) option =
  let rec sum_until_found_month (year : int) (n : int) (current_month : int) (sum : int) : (int * int) =
    match days_in_month (year, current_month) with
    | None -> (0, 0)
    | Some d -> 
      if sum + d >= n then (current_month, n - sum)
      else sum_until_found_month year n (current_month + 1) (sum + d)
  in
  let (m, d) = sum_until_found_month year n 1 0 in
  Some (year, m ,d)

(*  
Prints the Valid dates found by the dates in month function 
*)
let print_dates (result : (int * int * int) list option) : unit =
  match result with
  | None -> Printf.printf "Invalid month\n"
  | Some dates ->
      let rec loop lst =
        match lst with
        | [] -> ()
        | (y, m, d) :: rest ->
            Printf.printf "(%d, %d, %d)\n" y m d;
            loop rest
      in
      loop dates;;

  


Printf.printf "%b\n" (is_older ((2026, 1, 30), (2026, 7, 15)));;

match days_in_month (2026, 8) with
 | Some d -> Printf.printf "%i\n" d
 | None -> Printf.printf "Invalid month\n" ;;


let () = print_dates (dates_in_month (2026, 4)) ;;

let () =
  match num_of_days (2026, 1, 1) with
  | Some n -> Printf.printf "%d\n" n
  | None -> Printf.printf "Invalid date\n"
;;

let () =
  match nth_day (2026, 360) with
  | Some (y, m, d) -> Printf.printf "n=45: (%d, %d, %d)\n" y m d   (* expect (2026, 2, 14) *)
  | None -> Printf.printf "n=45: None\n"
;;

(*let valid_date (a : int) (b : int) (c : int) : bool =
  let valid_year = a >= 0 && a <= 3000 in
  let valid_month = b >= 1 && b <= 12 in
  let is_leap_year = (a mod 4 = 0 && a mod 100 <> 0) || a mod 400 = 0 in
  let days_in_month =
    if b = 1 || b = 3 || b = 5 || b = 7 || b = 8 || b = 10 || b = 12 then 31
    else if b = 4 || b = 6 || b = 9 || b = 11 then 30
    else if b = 2 then (if is_leap_year then 29 else 28)
    else 0
  in
  let valid_day = c >= 1 && c <= days_in_month in
  valid_year && valid_month && valid_day ;;*) 