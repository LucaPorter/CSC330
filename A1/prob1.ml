(*
Pt. 1, Problem 1: Takes 2 dates and returns true if the first is earlier than the second
  returns: bool
*)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) : bool =
  let (a1, b1, c1) = date1 in
  let (a2, b2, c2) = date2 in
  if a1 <> a2 then a1 < a2
  else if b1 <> b2 then b1 < b2
  else c1 < c2 ;;

(*
Pt. 2, Problem 1: Takes a year and month as input and returns the number of days in the month
  returns: int option
*)
let days_in_month ((a : int), (b : int)) : int option =
  let is_leap_year = (a mod 4 = 0 && a mod 100 <> 0) || a mod 400 = 0 in
  if b = 1 || b = 3 || b = 5 || b = 7 || b = 8 || b = 10 || b = 12 then Some 31
  else if b = 4 || b = 6 || b = 9 || b = 11 then Some 30
  else if b = 2 then (if is_leap_year then Some 29 else Some 28)
  else None;;

(*
  Pt. 3, Problem 1: Takes a year and month and finds all possible dates in that month
    returns: (int * int * int) list option
*)
let dates_in_month ((a : int), (b : int)) : (int * int * int) list option =
  let result = days_in_month (a, b) in
  if result = None then None
  else
    let d = Option.get result in
    let rec build_string (x : int) : (int * int * int) list =
      if x > d then []
      else (a, b, x) :: build_string (x + 1)
    in
    Some (build_string 1);;

(*
  Pt. 4, Problem 1:
    returns: int option
*)
let num_of_days ((a : int), (b : int), (c : int)) : int option =
  let rec sum_before_current_month (year : int) (month : int) (current_month : int) : int =
    if month >= current_month then 0
    else
      let result = days_in_month (year, month) in
      if result = None then 0
      else Option.get result + sum_before_current_month year (month + 1) current_month
  in
  let outer_result = days_in_month (a, b) in
  if outer_result = None then None
  else Some (sum_before_current_month a 1 b + c);;

(*
  Pt. 5, Problem 1: Given a year and 
*)
let nth_day ((year : int), (n : int)) : (int * int * int) option =
  let rec sum_until_found_month (year : int) (n : int) (current_month : int) (sum : int) : (int * int) =
    let result = days_in_month (year, current_month) in
    if result = None then (0, 0)
    else
      let d = Option.get result in
      if sum + d >= n then (current_month, n - sum)
      else sum_until_found_month year n (current_month + 1) (sum + d)
  in
  let (m, d) = sum_until_found_month year n 1 0 in
  Some (year, m, d);;

(*
Prints the Valid dates found by the dates in month function
*)
let print_dates (result : (int * int * int) list option) : unit =
  if result = None then Printf.printf "Invalid month\n"
  else
    let dates = Option.get result in
    let rec loop lst =
      if lst = [] then ()
      else
        let (y, m, d) = List.hd lst in
        Printf.printf "(%d, %d, %d)\n" y m d;
        loop (List.tl lst)
    in
    loop dates;;

Printf.printf "%b\n" (is_older ((2026, 1, 30), (2026, 7, 15)));;

let dm_result = days_in_month (2026, 8) in
if dm_result = None then Printf.printf "Invalid month\n"
else Printf.printf "%i\n" (Option.get dm_result) ;;

let () = print_dates (dates_in_month (2026, 4)) ;;

let () =
  let nd_result = num_of_days (2026, 1, 1) in
  if nd_result = None then Printf.printf "Invalid date\n"
  else Printf.printf "%d\n" (Option.get nd_result)
;;

let () =
  let (y, m, d) = Option.get (nth_day (2026, 360)) in
  Printf.printf "n=360: (%d, %d, %d)\n" y m d
;;