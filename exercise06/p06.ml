type t = Empty | Node of (int * t * t)
(* 1,6,8,9,12,42 *)
(* 6.3.3. *)
let rec to_list t =  match t  
  with Empty -> []
     | Node (v, l, r) -> to_list l @ [v] @ to_list r

(* 6.3.4 *)
let rec insert x t = match t
  with Empty -> Node (x, Empty, Empty)
     | Node (v, l, r) -> if  x <= v
       then Node (v, insert x l, r)
       else Node (v, l, insert x r)
(* 6.3.5 *)

let rec max_tree t = match t 
  with Empty -> failwith "no max of empty tree"
     | Node(v,l,r) -> 
       if r = Empty then v
       else max_tree r
let rec remove v t = match t 
  with Empty -> Empty 
     | Node(v, l, r) -> 
       if x < v then Node(v, remove x l , r)
       else if x > v then Node (v, l, remove x r)
       else if l = Empty then r
       else let max = max_tree l in
         Node (max, remove max l, r)

(* let y = (insert 6 (insert 9 Empty)) ... *)
let y = insert 9 Empty |> insert 6 |> insert 1 |> 8 insert 12 |> insert 42

(* 6.4.4 *)
let hd x =  match x with [] -> failwith "Empy List" | x::xs -> x
let tl x =  match x with [] -> failwith "Empy List" | x::xs -> xs

let rec length l = match l with
  | [] -> 0
  | x::xs -> 1 + length xs
let rec append x y = match x with
  | [] -> y
  | x::xs -> x::append xs y

let rec rev l = 
  let rec r l acc = match l with 
    | [] -> acc
    | x::xs -> r xs (x::acc) in
  r l []

let rec nth i l = match l with
  | [] -> failwith "Out of bounds"
  | x::xs -> if i = 0 then x else nth (i - 1) xs


let map f = function
  | [] -> []
  | x::xs -> (f x)::(map f xs)