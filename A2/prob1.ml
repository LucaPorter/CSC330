module type FSET = sig
  type 'a set
  exception EmptySet
  val empty_set : ('a -> 'a -> int) -> 'a set
  val is_empty : 'a set -> bool
  val size : 'a set -> int
  val insert : 'a set -> 'a -> 'a set
  val remove : 'a set -> 'a -> 'a set
  val union : 'a set -> 'a set -> 'a set
  val intersect : 'a set -> 'a set -> 'a set
  val from_list : ('a -> 'a -> int) -> 'a list -> 'a set
  val to_list : 'a set -> 'a list
  val map : ('a -> 'a) -> 'a set -> 'a set
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a
end

module FuncSet : FSET = struct
  type 'a set = { 
    cmp : 'a -> 'a -> int; 
    elements : 'a list 
    }
  exception EmptySet

  let empty_set cmp = { cmp = cmp; elements = [] }

  let is_empty s = (s.elements = [])

  let size s = 
    let rec count lst = 
      match lst with
      | [] -> count
      | _ :: r -> 1 + count r in 
    count s.elements

  let inset s x = 
    let rec ins lst = 
      match lst with 
      | [] -> [x]
      | h :: t -> 
        let c = s.cmp x h in 
        if c = 0 then lst
        else if c < 0 then x :: lst
        else h :: ins t
    in 
    { s with elements = ins s.elements }

  let remove s x = 
    let rec rem lst = 
      match lst with
      | [] -> raise EmptySet
      | h :: t -> if s.cmp x h = 0 then t else h :: rem t
    in
    { s with elements = rem s.elements }

  let union s1 s2 = 
    let rec combine lst acc = 
      match lst with
      | [] -> acc
      | h :: t -> combine t (insert acc h)
    in
    combine s2.elements s1

  let intersect s1 s2 =
    let rec mem lst x = 
      match lst with
      | [] -> false
      | h :: t -> if s1.cmp x h = 0 then true else mem t x
    in 
    let rec filter_inter lst = 
      match  lst with
      | [] -> []
      | h :: t -> if mem s2.elements h then h :: filter_inter t else filter_inter t
    in
    { s1 with elements = filter_inner s1.elements }

  let from_list cmp lst = 
    let rec build_set acc rem = 
      match rem with
      | [] -> acc
      | h :: t -> build_set (insert acc h) t
    in
    build_set (empty_set cmp) lst

  let to_list s = s.elements

  let map f s = 
    let rec build acc list = 
      match lst with 
      | [] -> acc
      | h :: t -> build (insert acc (f h))
    in
    build (empty_set s.cmp) s.elements

  let fold f acc s =
    let rec go acc lst =
      match lst with
      | [] -> acc
      | h :: t -> go (f acc h) t
    in 
    go acc s.elements

  let ( ++ ) s x = Function.insert s x