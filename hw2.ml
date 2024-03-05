exception NotImplemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

(** Recursive functions **)
let rec lrev l =
  match l with [] -> [] | first :: rest -> lrev rest @ [ first ]

let rec lrevrev l =
  match l with [] -> [] | first :: rest -> lrevrev rest @ [ lrev first ]

let rec lfoldl f e l =
  match l with [] -> e | first :: rest -> lfoldl f (f (first, e)) rest

(** Tail recursive functions  **)

let fact n =
  let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (n * acc) in
  fact_aux n 1

let fib n =
  let rec fib_aux n pprev prev =
    if n = 0 then pprev
    else if n = 1 then prev
    else fib_aux (n - 1) prev (pprev + prev)
  in
  fib_aux n 1 1

let alterSum t =
  let rec alterSum_aux t flag acc =
    match t with
    | [] -> acc
    | first :: rest ->
        if flag then alterSum_aux rest false (acc + first)
        else alterSum_aux rest true (acc - first)
  in
  alterSum_aux t true 0

let ltabulate n f =
  let rec ltabulate_aux n f acc =
    if n = 0 then acc else ltabulate_aux (n - 1) f (f (n - 1) :: acc)
  in
  ltabulate_aux n f []

let lfilter p l =
  let rec lfilter_aux p l acc =
    match l with
    | [] -> acc
    | first :: rest ->
        if p first then lfilter_aux p rest (acc @ [ first ])
        else lfilter_aux p rest acc
  in
  lfilter_aux p l []

let union s t =
  let rec exists el l =
    match l with
    | [] -> false
    | first :: rest -> if first = el then true else exists el rest
  in
  let rec union_aux l1 l2 acc =
    match (l1, l2) with
    | l1, [] -> l1 @ acc
    | [], l2 -> l2 @ acc
    | l1, el :: rest ->
        union_aux l1 rest (if exists el l1 then acc else el :: acc)
  in
  union_aux s t []

let inorder t =
  let rec inorder' t' post =
    match t' with
    | Leaf v -> v :: post
    | Node (l, v, r) -> inorder' l (v :: inorder' r post)
  in
  inorder' t []

let postorder t =
  let rec postorder' t' post =
    match t' with
    | Leaf v -> v :: post
    | Node (l, v, r) -> postorder' l (postorder' r (v :: post))
  in
  postorder' t []

let preorder t =
  let rec preorder' t' post =
    match t' with
    | Leaf v -> v :: post
    | Node (l, v, r) -> v :: preorder' l (preorder' r post)
  in
  preorder' t []

(** Sorting in the ascending order **)

let rec quicksort l =
  let rec partition pivot l =
    match l with
    | [] -> ([], [])
    | first :: rest ->
        let left, right = partition pivot rest in
        if first < pivot then (first :: left, right) else (left, first :: right)
  in
  match l with
  | [] -> []
  | first :: rest ->
      let left, right = partition first rest in
      quicksort left @ (first :: quicksort right)

let rec mergesort l =
  let split l =
    let rec split_aux left right cur flag =
      match cur with
      | [] -> (left, right)
      | first :: rest ->
          if flag then split_aux (first :: left) right rest false
          else split_aux left (first :: right) rest true
    in
    split_aux [] [] l true
  in
  let rec merge left right =
    match (left, right) with
    | left, [] -> left
    | [], right -> right
    | lfirst :: lrest, rfirst :: rrest ->
        if lfirst < rfirst then lfirst :: merge lrest right
        else rfirst :: merge left rrest
  in
  match l with
  | [] -> []
  | [ el ] -> [ el ]
  | l ->
      let left, right = split l in
      merge (mergesort left) (mergesort right)

(** Structures **)

module type HEAP = sig
  exception InvalidLocation
  type loc
  type 'a heap
  val empty : unit -> 'a heap
  val allocate : 'a heap -> 'a -> 'a heap * loc
  val dereference : 'a heap -> loc -> 'a
  val update : 'a heap -> loc -> 'a -> 'a heap
end

module type DICT = sig
  type key
  type 'a dict
  val empty : unit -> 'a dict
  val lookup : 'a dict -> key -> 'a option
  val delete : 'a dict -> key -> 'a dict
  val insert : 'a dict -> key * 'a -> 'a dict
end

module Heap : HEAP = struct
  exception InvalidLocation

  type loc = int
  type 'a htree = Empty | Leaf of 'a | Node of 'a htree * 'a * 'a htree
  type 'a heap = 'a htree * loc

  let empty () = (Empty, 0)
  let rec allocate h v = raise NotImplemented

  (* match h with
     | loc, tree ->
         let location = loc in
         let newTree = insert_into_tree tree location v in
         ((loc + 1, newTree), location) *)

  let dereference h l =
    let rec getDirections l =
      match l with
      | 0 -> raise InvalidLocation
      | 1 -> []
      | _ ->
          if l mod 2 = 1 then getDirections (l / 2) @ [ true ]
          else getDirections (l / 2) @ [ false ]
    in
    let directions = getDirections l in
    let rec getTarget tree directions =
      match (tree, directions) with
      | Leaf v, [] -> v
      | Node (_, v, _), [] -> v
      | Node (left, _, _), true :: rest -> getTarget left rest
      | Node (_, _, right), false :: rest -> getTarget right rest
      | _ -> raise InvalidLocation
    in
    match h with tree, _ -> getTarget tree directions

  let update _ _ _ = raise NotImplemented
end

module DictList : DICT with type key = string = struct
  type key = string
  type 'a dict = (key * 'a) list

  let empty _ = raise NotImplemented
  let lookup _ _ = raise NotImplemented
  let delete _ _ = raise NotImplemented
  let insert _ _ = raise NotImplemented
end

module DictFun : DICT with type key = string = struct
  type key = string
  type 'a dict = key -> 'a option

  let empty _ = raise NotImplemented
  let lookup _ _ = raise NotImplemented
  let delete _ _ = raise NotImplemented
  let insert _ _ = raise NotImplemented
end
