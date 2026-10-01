open Prob1

let cmp = Int.compare

let test_empty_set_basics () =
  let set = FuncSet.empty_set cmp in
  assert (FuncSet.is_empty set = true);
  assert (FuncSet.size set = 0);
  assert (FuncSet.to_list set = [])

let test_insert () =
  let set = FuncSet.insert (FuncSet.empty_set cmp) 1 in
  assert (FuncSet.size set = 1);
  assert (FuncSet.to_list set = [1]);
  assert (FuncSet.to_list (FuncSet.insert set (-1)) = [-1; 1]);
  assert (FuncSet.to_list (FuncSet.insert set 1) = [1])  (* duplicate, no change *)

let test_union_intersect () =
  let set = FuncSet.insert (FuncSet.empty_set cmp) 1 in
  let set2 =
    FuncSet.insert
      (FuncSet.insert (FuncSet.insert (FuncSet.empty_set cmp) 1) 2)
      (-1)
  in
  assert (FuncSet.to_list (FuncSet.union set set2) = [-1; 1; 2]);
  assert (FuncSet.to_list (FuncSet.intersect set set2) = [1])

let test_from_list () =
  let set = FuncSet.from_list cmp [100; 1; 2; -2; 2; 1] in
  assert (FuncSet.to_list set = [-2; 1; 2; 100])

let test_map_fold () =
  let set2 =
    FuncSet.insert
      (FuncSet.insert (FuncSet.insert (FuncSet.empty_set cmp) 1) 2)
      (-1)
  in
  assert (FuncSet.to_list (FuncSet.map (fun x -> x mod 2) set2) = [-1; 0; 1]);
  assert (FuncSet.fold (fun acc x -> acc + x) 0 set2 = 2)

let test_plusplus () =
  let open FuncSet in
  assert (to_list (empty_set cmp ++ 1 ++ 2 ++ 3 ++ 1) = [1; 2; 3])

let () =
  test_empty_set_basics ();
  test_insert ();
  test_union_intersect ();
  test_from_list ();
  test_map_fold ();
  test_plusplus ();
  print_endline "prob1: all tests passed"