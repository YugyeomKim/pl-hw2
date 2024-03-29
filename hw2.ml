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
  type 'a heap = 'a list
  let empty () = []
  let allocate h v =
    let new_loc = List.length h in
    (h @ [ v ], new_loc)

  let dereference h l =
    try List.nth h l
    with Failure _ | Invalid_argument _ -> raise InvalidLocation
  let rec update h l v =
    match h with
    | [] -> raise InvalidLocation
    | first :: rest ->
        if l = 0 then v :: rest else first :: update rest (l - 1) v
end

module DictList : DICT with type key = string = struct
  type key = string
  type 'a dict = (key * 'a) list

  let empty () = []
  let lookup d k =
    let el = List.filter (fun (k', _) -> k' = k) d in
    match el with [] -> None | (_, v) :: _ -> Some v
  let delete d k = List.filter (fun (k', _) -> k' <> k) d
  let insert d (k, v) =
    match lookup d k with
    | None -> (k, v) :: d
    | _ -> List.map (fun (k', v') -> if k' = k then (k, v) else (k', v')) d
end

module DictFun : DICT with type key = string = struct
  type key = string
  type 'a dict = key -> 'a option

  let empty () _ = None
  let lookup d k = d k
  let delete d k k' = if k' = k then None else d k'
  let insert d (k, v) k' = if k' = k then Some v else d k'
end
