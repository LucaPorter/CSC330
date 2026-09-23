(*
 * Pt. 1, Problem 1: Takes 2 dates and returns true if the first comes earlier than the second
 *   returns: bool
 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) : bool =
  let (a1, b1, c1) = date1 in
  let (a2, b2, c2) = date2 in
  if a1 <> a2 then a1 < a2
  else if b1 <> b2 then b1 < b2
  else c1 < c2 ;;

(*
 * Pt. 2, Problem 1: Takes a year and month as input and returns the number of days in the month
 *   returns: int option
 *)
let days_in_month ((a : int), (b : int)) : int option =
  if a <= 0 || b <= 0 then None
  else
    let is_leap_year = (a mod 4 = 0 && a mod 100 <> 0) || a mod 400 = 0 in
    if b = 1 || b = 3 || b = 5 || b = 7 || b = 8 || b = 10 || b = 12 then Some 31
    else if b = 4 || b = 6 || b = 9 || b = 11 then Some 30
    else if b = 2 then (if is_leap_year then Some 29 else Some 28)
    else None;;

(*
 * Pt. 3, Problem 1: Takes a year and month and finds all valid dates in that month
 *   returns: (int * int * int) list option
 *)
let dates_in_month ((a : int), (b : int)) : (int * int * int) list option =
  let result = days_in_month (a, b) in
  if result = None then None
  else
    let d = Option.get result in
    (* Builds the triples (a, b, 1) through (a, b, d). *)
    let rec build_string (x : int) : (int * int * int) list =
      if x > d then []
      else (a, b, x) :: build_string (x + 1)
    in
    Some (build_string 1);;

(*
 * Pt. 4, Problem 1: Takes in a date and returns the number of days from the beginning of the year until the date, inclusive
 *   returns: int option
 *)
let num_of_days ((a : int), (b : int), (c : int)) : int option =
  (* Sums the lengths of all months strictly before current_month. *)
  let rec sum_before_current_month (year : int) (month : int) (current_month : int) : int =
    if month >= current_month then 0
    else
      let result = days_in_month (year, month) in
      if result = None then 0
      else Option.get result + sum_before_current_month year (month + 1) current_month
  in
  let outer_result = days_in_month (a, b) in
  if outer_result = None then None
  else
    let max_days = Option.get outer_result in
    if c <= 0 || c > max_days then None
    else Some (sum_before_current_month a 1 b + c);;

(*
 * Pt. 5, Problem 1: Given a year and number n, returns the date that coresponds to the n'th day of the year
 *   returns: (int * int * int) option
 *)
let nth_day ((year : int), (n : int)) : (int * int * int) option =
  let total_days = if days_in_month (year, 2) = Some 29 then 366 else 365 in

  if year <= 0 || n <= 0 || n > total_days then None
  else
    (* Walks months forward, accumulating sum (days before current_month),
     * until n falls within current_month. *)
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
