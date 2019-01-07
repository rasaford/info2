let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n <> 1 && is_not_divisor 2

let rec print_list = function 
  | [] -> Printf.printf "\n"; ()
  | x::xs -> let () =  Printf.printf "%d, " x in print_list xs



let rec writeable b p a =
  if b = (p + 2 * a * a) then 
    let () = Printf.printf "%d\n" b in true
  else if p > b || (2 * a * a) > b then false
  else writeable b p (a+1)


let fst_question (a : int)  = 
  let nums = List.init a (fun x -> x) in
  let primes = List.filter is_prime nums in
  let pairs = List.map (fun x -> List.init a (fun y -> x + 2 * y * y)) primes in
  let pairs = List.fold_left (fun a c -> a @ c) [] pairs in
  let wo = List.filter (fun x -> List.exists (fun y -> x = y) pairs) nums in
  let res = List.filter (fun x -> not (is_prime x) && x > 1 && (x mod 2 <> 0)) wo in
  print_list res; ()

let three n = (n * (n + 1)) / 2
let five n = (n *(3 * n - 1)) / 2
let six n = n * (2 * n - 1)

let rec second_question tn pn hn = 
  let t = three tn in let p = five pn in let h = six hn in
  if t > 1000000000000 then ()
  else if t = p && p = h then 
    let () = (Printf.printf "Tn: %d\n" t) in
    second_question (tn + 1)  (pn + 1) (hn + 1)
  else if t <= p && t <= h then
    second_question (tn +1) pn hn
  else if p <= t && p <= h then 
    second_question tn (pn +1) hn 
  else 
    second_question tn pn (hn +1)


let () = 
  let () = fst_question 100 in
  let () = second_question 1 1 1 in
  ()