open Prob2

let test_zip () =
  assert (zip [1; 2; 3] ["a"; "b"; "c"] = Some [(1, "a"); (2, "b"); (3, "c")]);
  assert (zip [1; 2] ["a"] = None)

let test_flatmap () =
  assert (flatmap (fun x -> [x; -x]) [1; 2; 3] = [1; -1; 2; -2; 3; -3])

let test_flatmap_opt () =
  assert (flatmap_opt (fun x -> if x mod 2 = 0 then Some [x; -x] else None) [2; 4]
          = Some [2; -2; 4; -4]);
  assert (flatmap_opt (fun x -> Some [x]) [] = Some []);
  assert (flatmap_opt (fun x -> if x mod 2 = 0 then Some [x; -x] else None) [1; 2; 4]
          = None)

let t =
  [ "table1", [ ("str", Str "hello")
              ; ("int", Int 5)
              ; ("table", Table ["a", Str "x"; "b", Str "Y"])
              ; ("list", ListOf [Int 1; Int 2]) ]
  ; "table2", [ ("something", Str "maybe") ] ]

let test_get_strings () =
  assert (get_strings t =
    [("table1", "hello"); ("table1", "x"); ("table1", "Y"); ("table2", "maybe")])

let test_is_uppercase () =
  assert (is_uppercase "HELLO" = true);
  assert (is_uppercase "Hello" = false)

let test_get_uppercase_strings () =
  assert (get_uppercase_strings_1 t = ["Y"]);
  assert (get_uppercase_strings_2 t = ["Y"])

let test_omatch_success () =
  let p = TablePattern
    [ "", StringPattern "hello"
    ; "int", BindPattern "x"
    ; "", TablePattern [ "", BindPattern "y"; "b", WildcardPattern ]
    ; "", WildcardPattern ]
  in
  assert (omatch p (Table (snd @@ List.hd t)) = Some [("x", Int 5); ("y", Str "x")])

let test_omatch_failure () =
  let p = TablePattern
    [ "", StringPattern "hello"
    ; "", BindPattern "x"
    ; "", TablePattern [ "a", BindPattern "y"; "c", WildcardPattern ] ]
  in
  assert (omatch p (Table (snd @@ List.hd t)) = None)

let test_ocheck () =
  assert (ocheck t = true);
  let t' = (List.hd t) :: ["table1", []] in
  assert (ocheck t' = false)

let test_otypecheck () =
  assert (otypecheck t =
    Some [ TableType [ StrType; IntType; TableType [ StrType; StrType ]; ListType IntType ]
         ; TableType [ StrType ] ]);
  assert (otypecheck (t @ ["table3", ["foo", ListOf [Int 1; Str "2"]]]) = None)

let () =
  test_zip ();
  test_flatmap ();
  test_flatmap_opt ();
  test_get_strings ();
  test_is_uppercase ();
  test_get_uppercase_strings ();
  test_omatch_success ();
  test_omatch_failure ();
  test_ocheck ();
  test_otypecheck ();
  print_endline "prob2: all tests passed"