type ovalue =
  | Str of string
  | Int of int
  | Table of okeyvalue list
  | ListOf of ovalue list
and okeyvalue = string * ovalue
and otable = string * okeyvalue list
and otoml = otable list

type opattern =
  | WildcardPattern
  | BindPattern of string
  | StringPattern of string
  | IntPattern of int
  | TablePattern of (string * opattern) list
  | ListPattern of opattern list

let rec zip (xs : a' list) (ys : a' list)  : (a' * b') list option =
  match (xs ys) with
  | ([], []) ->  Some []
  | ([], _ :: _) -> None
  | (_ :: _. []) -> None
  | (x :: xs', y :: ys') -> 
    match zip xs' ys' with
    | None -> None 
    | Some rest -> Some ((x, y) :: rest)

let rec flatmap (f : a' -> b' list) (xs : a' list) : b' list =
  match xs with
  | [] -> []
  | x : xs' -> (f x) @ (flatmap f xs')

let rec flatmap_opt (f : a' list option) (xs : a' list) : b' list option =
  match xs with
  | [] -> Some []
  | x :: xs' -> 
    match f x with 
    | None -> None
    | Some ys -> 
      match flatmap_opt f xs' with 
      | None -> None
      | Some rest -> Some ys @ rest

let rec strings_in_value (table_name : string) (v : ovalue) : (string * string) list =
  match v with 
  | Str  s -> [(table_name, s)]
  | Int _ -> []
  | Table kvs -> flatmap (fun (_, v') -> strings_in_value table_name v') kvs
  | ListOf flatmap (strings_in_value table_name) vs 

let strings_in_table ((name, kvs) : otable) : (string * string) list =
  flatmap (fun (_, v) -> strings_in_value name v) kvs

let get_strings (t : otaml) : (string * string) list =
  flatmap strings_in_table t

let is_uppercase (s : string) : bool =
  String.uppercase s = s

let ( % ) f g x = f (g x)

let extract_uppercase (pairs : (strings * string) list) : string list = 
  flatmap (fun (_, s) -> if is_uppercase s then [s] else []) pairs

let get_uppercase_strings_1 (t : otaml) : string list = 
  (extract_uppercase % get_strings) t

let get_uppercase_strings_2 (t : otoml) : string list =
  t |> get_strings |> extract_uppercase

let rec omatch (pat : opattern) (v : ovalue) : (string * ovalue) list option =
  match (pat, v) with
  | (WildcardPattern, _) -> Some []
  | (BindPattern name, _) -> Some [(name, v)]
  | (StringPattern s, Str s') -> if s = s' then Some [] else None
  | (IntPattern i, Int i') -> if i = i' then Some [] else None
  | (TablePattern pats, Table kvs) ->
    match zip pats kvs with 
    | None -> None
    | Some paitred ->
      flatmap_opt (fun ((name_pat, p), (key, v')) ->
        if name_pat = "" || name_pat = key then omatch p v' else None ) paired
  | (ListPattern pats, ListOf vs) ->
    match zip pats vs with
     | None -> None
     | Some paired -> flatmap_opt (fun (p, v') -> omatch p v') paired
  | (_, _) -> None

let rec contains (x : string) (xs : string list) : bool =
  match xs with
  | [] -> false
  | h :: t -> if h = x then true else contains x t

let rec no_dup (xs : string list) : bool =
  match xs with
  | [] -> true
  | x :: xs' -> if contains x xs' then false else no_dup xs'

let rec value_ok (v : ovalue) : bool =
  match v with
  | Str _ | Int _ -> true
  | Table kvs -> keyvalues_ok kvs
  | ListOf vs -> all_values_ok vs

and keyvalues_ok (kvs : okeyvalue list) : bool =
  let keys = flatmap (fun (k, _) -> [k]) kvs in
  no_dup keys && all_values_ok (flatmap (fun (_, v) -> [v]) kvs)

and all_values_ok (vs : ovalue list) : bool =
  match vs with
  | [] -> true
  | v :: vs' -> value_ok v && all_values_ok vs'

let ocheck (t : otoml) : bool =
  let names = flatmap (fun (name, _) -> [name]) t in
  no_dup names &&
  let rec tables_ok tbls = match tbls with
     | [] -> true
     | (_, kvs) :: rest -> keyvalues_ok kvs && tables_ok rest
   in tables_ok t

type otype =
  | StrType
  | IntType
  | ListType of otype
  | TableType of otype list

let rec type_of_value (v : ovalue) : otype option =
  match v with
  | Str _ -> Some StrType
  | Int _ -> Some IntType
  | Table kvs ->
    (match type_of_keyvalues kvs with
     | None -> None
     | Some types -> Some (TableType types))
  | ListOf vs ->
    (match type_of_list_elems vs with
     | None -> None
     | Some t -> Some (ListType t))

and type_of_keyvalues (kvs : okeyvalue list) : otype list option =
  flatmap_opt (fun (_, v) ->
    match type_of_value v with
    | None -> None
    | Some t -> Some [t]
  ) kvs

and type_of_list_elems (vs : ovalue list) : otype option =
  match vs with
  | [] -> None
  | v :: vs' ->
    match type_of_value v with
     | None -> None
     | Some t0 ->
       let rec check_rest lst =
         match lst with
         | [] -> Some t0
         | v' :: tail ->
           match type_of_value v' with
            | Some t' when t' = t0 -> check_rest tail
            | _ -> None
       in check_rest vs'

let otypecheck (t : otoml) : otype list option =
  flatmap_opt (fun (_, kvs) ->
    match type_of_keyvalues kvs with
    | None -> None
    | Some types -> Some [TableType types]
  ) t