
module type Ring = sig
  type t
  val zero : t
  val one : t
  val compare : t -> t -> int
  val to_string : t -> string
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type Matrix = sig
  type elem
  type t
  val create : int -> int -> t
  val identity : int -> t
  val from_rows : elem list list -> t
  val to_string : t -> string
  val set : int -> int -> elem -> t -> t
  val get : int -> int -> t -> elem
  val transpose : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)
(* Assignment 10.2 [20 Points] *)

module IntRing = struct 
  type t = int
  let zero = 0
  let one = 1
  let compare a b = a - b
  let to_string e = Printf.sprintf "%d" e
  let add a b = a + b
  let mul a b = a * b
end

module FloatRing = struct 
  type t = float
  let zero = 0.
  let one = 1.
  let compare a b = if a < b then -1 else if a > b then 1 else 0
  let to_string e = Printf.sprintf "%g." e
  let add a b = a +. b
  let mul a b = a *. b
end

module type FiniteRing = sig
  include Ring
  val elems : t list
end

module BoolRing = struct
  type t = bool
  let elems = [true; false]
  let zero = false
  let one = true
  let compare a b = match (a, b) with
    | (true, true) ->  0
    | (true, false) -> 1
    | (false, true) -> -1
    | (false, false) -> 0
  let to_string e = Printf.sprintf "%B" e
  let add a b = a || b
  let mul a b = a && b
end

module SetRing (R: FiniteRing) : Ring with type t = R.t list = struct
  type t = R.t list
  let zero = []

  let one = R.elems

  let to_string set = 
    let res = List.fold_left 
        (fun a c -> a ^ (R.to_string c) ^ ", ") "{" set in
    (String.sub res 0 ((String.length res) -2)) ^ "}"

  (* Set union *)
  let add a b = a @ (List.filter (fun x -> not (List.exists (fun y -> x = y) a)) b)

  (* Set intersection *)
  let mul a b = List.filter (fun x -> List.exists (fun y -> x = y) b) a

  let compare a b = 
    let sort_to_string l = to_string (List.sort (fun x y -> R.compare x y) l) in
    String.compare (sort_to_string a) (sort_to_string b)
end

module DenseMatrix (R: Ring) : Matrix with type elem = R.t and type t = R.t list list = struct 
  type elem = R.t
  type t = R.t list list

  let create rows cols = List.init rows (fun x -> List.init cols (fun x -> R.zero))

  let identity size = 
    List.init size (fun r -> List.init size (fun c -> if r = c then R.one else R.zero))

  let from_rows rows = rows

  let to_string m = List.map (fun x -> List.map R.to_string x 
                                       |> String.concat " ")  m
                    |> String.concat "\n"

  let rec list_get (i : int) l = match l with 
    | [] -> failwith "index out of bounds"
    | x::xs -> if i = 0 then x else list_get (i - 1) xs

  let rec list_set (i: int) v l = match l with 
    | [] -> []
    | x::xs -> if i = 0 
      then v::xs
      else x::(list_set (i - 1) v xs)

  let set r c v m = list_set r (list_set c v (list_get r m)) m
  let get r c m = list_get c (list_get r m)

  let transpose ma =
    let n = List.length ma in let m = List.hd ma |> List.length in
    List.init n (fun x -> x)
    |> List.fold_left (fun res i -> 
        List.init m (fun x -> x)
        |> List.fold_left (fun res j -> 
            set j i (get i j ma) res
          ) res
      ) (create m n)

  let add a b = List.map2 (fun x y -> List.map2 (fun x y -> R.add x y) x y) a b

  let mul a b = 
    let n = List.length a in let m = List.hd a |> List.length in
    let res = List.init n (fun x -> x)
              |> List.fold_left (fun res i -> 
                  List.init m (fun x -> x) 
                  |> List.fold_left (fun res j -> 
                      set i j (
                        List.init m (fun x -> x) 
                        |> List.fold_left (fun res k -> 
                            R.add (R.mul (get i k a) (get k j b)) res
                          ) R.zero
                      ) res
                    ) res
                ) (create (List.length a) (List.hd b |> List.length)) in
    (* to_string res; *)
    res
end 

module SparseMatrix (R: Ring) : Matrix with type elem = R.t and type t = (int * int * (int -> int -> R.t)) = struct 
  type elem = R.t
  (* n, m, Matrix function *)
  type t = (int * int * (int -> int -> R.t))

  let fst t = let (x, _, _) = t in x

  let sec t = let (_, x, _) = t in x

  let trd t = let (_, _, x) = t in x

  let to_string ma = 
    let n = fst ma in let m = sec ma in let matrix = trd ma in 
    List.init n (fun x -> x)
    |> List.fold_left (fun str i -> 
        String.concat (if i = 0 then "" else "\n")
          [str; List.init m (fun x -> x) 
                |> List.fold_left (fun str j -> 
                    String.concat (if j = 0 then "" else " ")
                      [str; R.to_string (matrix i j)]
                  ) ""]
      ) ""

  let set r c v m = (fst m, sec m, (fun x y -> if x = r && y = c then v else (trd m) x y))

  let get r c m = (trd m) r c

  let create n m = (n, m, fun x y -> R.zero)

  let identity n = 
    let rec diag n acc = match n with  
      | 0 -> set 0 0 R.one acc
      | i -> diag (i - 1) (set i i R.one acc) in
    diag n (create n n)

  let from_rows l = 
    let n = (List.length l) in let m = (List.hd l |> List.length) in
    let rec cols l i j m = match l with  
      | [] -> m
      | x::xs -> cols xs i (j + 1) (if x != R.zero then set i j x m else m) in
    let rec rows l i m = match l with 
      | [] -> m
      | x::xs -> rows xs (i + 1) (cols x i 0 m) in
    rows l 0 (create n m)

  let transpose ma = 
    let n = fst ma in let m = sec ma in
    let ma = trd ma in
    List.init n (fun x -> x)
    |> List.fold_left (fun res i -> 
        List.init m (fun x -> x)
        |> List.fold_left (fun res j -> 
            set j i (ma i j) res
          ) res
      ) (create m n)


  let add a b = 
    let n = fst a in let m = sec a in 
    let a = trd a in let b = trd b in
    List.init n (fun x -> x)
    |> List.fold_left (fun res i -> 
        List.init m (fun x -> x) 
        |> List.fold_left (fun res j -> 
            set i j (R.add (a i j) (b i j)) res
          ) res
      ) (create n m)

  let mul am bm =
    let n = fst am in let m = sec am in 
    let a = trd am in let b = trd bm in
    List.init n (fun x -> x)
    |> List.fold_left (fun res i -> 
        List.init m (fun x -> x) 
        |> List.fold_left (fun res j -> 
            set i j (
              List.init m (fun x -> x) 
              |> List.fold_left (fun res k -> 
                  R.add (R.mul (a i k) (b k j)) res
                ) R.zero
            ) res
          ) res
      ) (create (fst am) (sec bm))
end



(* todo ... *)

(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let (|=) a b =
  List.sort compare a = List.sort compare b

let check_string_representation s elems =
  if String.length s < 2 then false else
  if String.get s 0 <> '{' then false else
  if String.get s (String.length s - 1) <> '}' then false else
    String.sub s 1 (String.length s - 2)
    |> String.split_on_char ','
    |> List.map String.trim
    |> (|=) elems

let tests =
  (****************************
   * tests for 10.2 (IntRing) :
   * NOTE: Comment tests until you have completed your implementation of IntRing
  *)
  let implementsRingSignature (module M : Ring) = true in
  [
    __LINE_OF__ (fun () -> implementsRingSignature (module IntRing));
    __LINE_OF__ (fun () -> IntRing.compare 9 10 < 0 && IntRing.compare 10 9 > 0 && IntRing.compare 10 10 = 0);
    __LINE_OF__ (fun () -> IntRing.add 10 IntRing.zero = 10);
    __LINE_OF__ (fun () -> IntRing.mul 10 IntRing.one = 10);
    __LINE_OF__ (fun () -> IntRing.to_string 10 = "10");
  ] @

  (******************************
   * tests for 10.2 (FloatRing) :
   * NOTE: Comment tests until you have completed your implementation of FloatRing
  *)
  let implementsRingSignature (module M : Ring) = true in
  [
    __LINE_OF__ (fun () -> implementsRingSignature (module FloatRing));
    __LINE_OF__ (fun () -> FloatRing.compare 9.5 10.0 < 0 && FloatRing.compare 10.0 9.5 > 0 && FloatRing.compare 10.0 10.0 = 0);
    __LINE_OF__ (fun () -> FloatRing.add 10.0 FloatRing.zero = 10.0);
    __LINE_OF__ (fun () -> FloatRing.mul 10.0 FloatRing.one = 10.0);
    __LINE_OF__ (fun () -> FloatRing.to_string 10.0 = "10.");
  ] @

  (*****************************
   * tests for 10.2 (BoolRing) :
   * NOTE: Comment tests until you have completed your implementation of BoolRing
  *)

  let implementsFiniteRingSignature (module M : FiniteRing) = implementsRingSignature (module M) in
  [
    __LINE_OF__ (fun () -> implementsFiniteRingSignature (module BoolRing));
    __LINE_OF__ (fun () -> BoolRing.compare BoolRing.zero BoolRing.one < 0 && BoolRing.compare BoolRing.one BoolRing.zero > 0 && BoolRing.compare BoolRing.zero BoolRing.zero = 0);
    __LINE_OF__ (fun () -> BoolRing.add true BoolRing.zero = true && BoolRing.add false BoolRing.zero = false);
    __LINE_OF__ (fun () -> BoolRing.mul true BoolRing.one = true && BoolRing.mul false BoolRing.one = false);
    __LINE_OF__ (fun () -> BoolRing.to_string true = "true");
    __LINE_OF__ (fun () -> BoolRing.elems |= [true;false]);
  ] @

  (****************************
   * tests for 10.2 (SetRing) :
   * NOTE: Comment tests until you have completed your implementation of SetRing
  *)

  let module TestRing : FiniteRing with type t = char = struct
    let cfrom x = (int_of_char x) - (int_of_char 'a')
    let cto x = char_of_int (x mod 4 + int_of_char 'a')

    type t = char
    let zero = 'a'
    let one = 'd'
    let compare = Pervasives.compare
    let to_string c = Printf.sprintf "'%c'" c
    let add a b = (cfrom a) + (cfrom b) |> cto
    let mul a b = (cfrom a) * (cfrom b) |> cto
    let elems = ['a'; 'b'; 'c'; 'd']
  end in
  let module SR = SetRing (TestRing) in
  [
    __LINE_OF__ (fun () -> SR.zero = [] && SR.one |= ['a'; 'b'; 'c'; 'd']);
    __LINE_OF__ (fun () -> SR.compare ['b';'d'] ['a'] > 0);
    __LINE_OF__ (fun () -> SR.compare ['c';'b'] ['c';'d'] < 0);
    __LINE_OF__ (fun () -> SR.compare ['a';'d'] ['d';'a'] = 0);
    __LINE_OF__ (fun () -> SR.add ['a';'b'] ['c';'b'] |= ['a';'b';'c']);
    __LINE_OF__ (fun () -> SR.add ['b';'d'] SR.zero |= ['b';'d']);
    __LINE_OF__ (fun () -> SR.mul ['a';'b'] ['c';'b'] |= ['b']);
    __LINE_OF__ (fun () -> SR.mul ['a';'b'] SR.one |= ['a';'b']);
    __LINE_OF__ (fun () -> check_string_representation (SR.to_string SR.one) ["'a'";"'b'";"'c'";"'d'"]);
  ] @

  (********************************
   * tests for 10.2 (DenseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of DenseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
  *)

  let module DM = DenseMatrix (IntRing) in
  let dm0 = DM.from_rows [[4;-2;1];[0;3;-1]] in
  let dm1 = DM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_dense m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = DM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_dense (DM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_dense (DM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.set 1 0 7 (DM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.transpose dm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_dense (DM.add dm0 dm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_dense (DM.mul dm0 dm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (DM.to_string dm0) = "4 -2 1\n0 3 -1");
  ] @

  (*********************************
   * tests for 10.2 (SparseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of SparseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
  *)
  let module SM = SparseMatrix (IntRing) in
  let sm0 = SM.from_rows [[4;-2;1];[0;3;-1]] in
  let sm1 = SM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_sparse m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = SM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_sparse (SM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_sparse (SM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.set 1 0 7 (SM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.transpose sm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.add sm0 sm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_sparse (SM.mul sm0 sm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (SM.to_string sm0) = "4 -2 1\n0 3 -1");
  ] @
  []


let () =
  let rec input_lines ch =
    (try Some (input_line ch) with _ -> None) (* catch stupid EOF exception *)
    |> function Some line -> line :: input_lines ch | None -> []
  in
  let lines = input_lines (open_in __FILE__) in
  let open List in
  let open Printf in
  let fail l =
    let line = nth lines (l-1) in
    let test = String.sub line 25 (String.length line - 27) in
    printf "test \027[31;m%s\027[0;m (line %d) failed!\n" test l;
  in
  let test (l, t) =
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)


