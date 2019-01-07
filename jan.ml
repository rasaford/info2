let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n <> 1 && is_not_divisor 2

let rec print_list l = match l with
  | [] -> Printf.printf "\n"
  | x::xs -> Printf.printf "%d, " x; print_list xs

let writeable a =
  let primes = List.init a (fun x -> x) 
               |> List.filter is_prime  in
  let ass = List.init (a / 2) (fun x -> x)
            |> List.filter (fun x -> x >= 1)
            |> List.map (fun x -> 2 * x * x) in
  List.exists (fun p -> 
      List.exists (fun c -> a = (p + c)) ass
    ) primes

let fst_question (a : int) = 
  Printf.printf "First Question: \n";
  let nums =  List.init a (fun x -> x)
              |> List.map (fun x -> 2 * x + 1)
              |> List.filter (fun x -> not (is_prime x))
              |> List.filter (fun x -> x > 1)
              |> List.filter (fun x -> not (writeable x)) in
  print_list nums 


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
  let () = fst_question 5000 in
  let () = second_question 1 1 1 in
  ()
