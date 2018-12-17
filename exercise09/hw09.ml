let todo _ = failwith "TODO"

type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

(* 9.3 - 1 *)
let read_notes filename = 
  let file = open_in filename in
  let rec read_child children = 
    try 
      let line = input_line file in 
      match String.split_on_char ':' line with 
      | [name; behaviour] -> let beh = (match behaviour with 
          | "nice" -> Nice
          | "naughty" -> Naughty
          | _ -> raise (Invalid_file_format(filename))) in
        read_child ((name, beh)::children)
      | _ -> raise (Invalid_file_format(filename))
    with End_of_file -> children in
  try 
    let children = read_child [] in
    close_in file;
    children
  with e -> close_in file; raise e

(* 9.3 - 2 *)
let read_wishlist filename = 
  let file = open_in filename in
  let rec read_wish wishes =
    try 
      let line = input_line file in
      match String.split_on_char ':' line with
      | [wish; importance] -> let importance = (try 
                                                  int_of_string importance
                                                with _ -> raise (Invalid_file_format(filename))) in
        if importance < 1 || importance > 100 || 0 = String.length wish 
        then raise (Invalid_file_format(filename)) 
        else read_wish ((wish, importance)::wishes)
      | _ -> raise (Invalid_file_format(filename))
    with End_of_file -> wishes in
  try 
    let wishes = read_wish [] in
    close_in file;
    wishes
  with e -> close_in file; raise e


(* 9.3 - 3 *)
let load_catalogue filename = 
  let file = open_in filename in
  let rec read_toy toys =
    try 
      let line = input_line file in
      match String.split_on_char ':' line with
      | [toy; weight] -> let weight = (try 
                                         int_of_string weight
                                       with _ -> raise (Invalid_file_format(filename))) in
        if weight < 0 || 0 = String.length toy
        then raise (Invalid_file_format(filename)) 
        else read_toy ((toy, weight)::toys)
      | _ -> raise (Invalid_file_format(filename))
    with End_of_file -> toys in
  try 
    let toys = read_toy [] in
    close_in file;
    toys
  with e -> close_in file; raise e

(* 9.3 - 4 *)

let write_list (filename : string) (list : string list) =
  let file = open_out filename in
  List.iter (fun p -> Printf.fprintf file "%s\n" p) list;
  close_out file

(* 9.3 - 5 *)
let write_letter filename =
  let file = open_out filename in
  Printf.fprintf file "All work and no Play makes Jack a dull boy\n";
  close_out file

(* 9.3 - 6 *)
let rec assemble_cargo wishlist toys acc = match wishlist with
  | [] -> acc
  | (name, importance)::ws -> let toy = List.find_opt (fun t -> (fst t) = name) toys in
    match toy with
    | Some((_, weight)) -> assemble_cargo ws toys ((name, importance, weight)::acc)
    | None -> assemble_cargo ws toys acc

let run_santas_factory capacity alg = 
  let toys = load_catalogue "toys_catalogue.txt" in
  let children = read_notes "santas_notes.txt" in
  let nice_c = List.filter (fun c -> let (name, status) = c in
                             match status with 
                             | Nice -> true 
                             | Naughty -> write_letter (Printf.sprintf "%s_letter.txt" name); false) children in
  let rec distribute children = match children with 
    | [] -> ()
    | (name, _)::cs -> 
      let wishlist = read_wishlist (Printf.sprintf "%s_wishlist.txt" name) in
      let cargo = assemble_cargo wishlist toys [] in
      let selection = alg cargo capacity in
      write_list (Printf.sprintf "%s_presents.txt" name) selection;
      distribute cs in
  distribute nice_c

(* 9.3 - 7 *)
let value list = List.fold_left (fun a (_, i, _) -> a + i) 0 list

let rec knap items cap acc = match items with
  | [] -> acc
  | (t, v, w)::xs -> if w > cap 
    then knap xs cap acc
    else let inside = knap xs (cap - w) acc in
      let outside = knap xs cap acc in
      if v + (value inside) > value outside 
      then (t, v, w)::inside 
      else outside

let knapsack (list : (string * int * int) list) cap = 
  List.map (fun (t, _, _) -> t) (knap list cap [])




(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a933_ex1 = ["penguin doll",1; "ocaml book",2; "time machine",53; "bike",7; "barbie's dream house",5;
                "guitar",6; "colorful pencils",2; "socks",1; "shawl",2; "karaoke machine",13; "superman action doll set",3;
                "guinea pig",3; "horse",10; "unicorn",8; "sand toys",4; "soccer shoes",3]

(*****************************************************************************)
(* TESTS [do not change] *)
let (=^) a b =
  (List.sort compare a) = (List.sort compare b)
let (=|) a b =
  let a = List.sort_uniq (fun x y -> compare (fst x) (fst y)) a in
  let b = List.sort_uniq (fun x y -> compare (fst x) (fst y)) b in
  a = b
let check_throws e f =
  try f (); false with e' -> e' = e

let check_file filename content =
  let file = open_in filename in
  let rec read acc =
    try
      read ((input_line file)::acc)
    with End_of_file -> acc
  in
  let c = read [] in
  close_in file;
  (List.sort_uniq compare c) = (List.sort_uniq compare content)

let check_letter filename =
  let file = open_in filename in
  let rec read () =
    try
      let line = input_line file in
      if line <> "" then true else
        read ()
    with End_of_file -> false
  in
  let r = read () in
  close_in file;
  r

let raise' = function Failure f ->
  Printf.printf "TEST FAILURE: %s\n" f;
  raise (Failure f)
                    | e -> raise e

let check_run_santas_factory () =
  let test_selection_alg wishes capacity =
    if capacity <> 13 then raise' (Failure "wrong capacity passed to selection_alg");
    (match List.find_opt (fun (t,_,_) -> t = "ocaml book") wishes with
     | None -> raise' (Failure "wrong list passed to selection_alg")
     | Some (_,_,w) -> if w <> 2 then raise' (Failure "wrong list passed to selection_alg"));
    match List.sort (fun (_,i,_) (_,j,_) -> compare j i) wishes with
    | (w1,_,_)::(w2,_,_)::_ -> [w1;w2]
    | _ -> raise' (Failure "wrong list passed to selection_alg")
  in
  ignore(run_santas_factory 13 test_selection_alg);
  if not (check_letter "marta_letter.txt") then raise (Failure "no correct letter produced for marta");
  if not (check_letter "bruno_letter.txt") then raise (Failure "no correct letter produced for bruno");
  if not (check_file "frida_presents.txt" ["colorful pencils";"ocaml book"]) then raise (Failure "no correct present list produced for frida");
  if not (check_file "tommy_presents.txt" ["sand toys";"superman action doll set"]) then raise (Failure "no correct present list produced for tommy");
  if not (check_file "caren_presents.txt" ["penguin doll";"unicorn"]) then raise (Failure "no correct present list produced for caren");
  true

let tests = [
  (* tests for 9.3 - 1 *)
  __LINE_OF__ (fun () -> (read_notes "examples/santas_notes.txt") =| ["tommy",Nice;"bruno",Naughty;"frida",Nice;"caren",Nice;"marta",Naughty]);
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  (* tests for 9.3 - 2 *)
  __LINE_OF__ (fun () -> (read_wishlist "examples/frida_wishlist.txt") =| ["ocaml book",10;"horse",3;"colorful pencils",12]);
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  (* tests for 9.3 - 3 *)
  __LINE_OF__ (fun () -> (load_catalogue "examples/toys_catalogue.txt") =| a933_ex1);
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  (* tests for 9.3 - 4 *)
  __LINE_OF__ (fun () -> let l = ["socks";"colorful pencils";"horse"] in let fn = "examples/testout_list1.txt" in write_list fn l; check_file fn l);
  (* tests for 9.3 - 5 *)
  __LINE_OF__ (fun () -> let fn = "examples/testout_letter1.txt" in write_letter fn; check_letter fn);
  (* tests for 9.3 - 6 *)
  __LINE_OF__ (fun () -> check_run_santas_factory ());
  (* tests for 9.3 - 7 *)
  __LINE_OF__ (fun () -> (
        knapsack ["a",5,4; "b",2,2; "b",2,2; "d",4,5; "b",2,2; "e",8,2] 10) 
        =^ ["a";"b";"b";"e"]);
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "a",5,4; "c",11,6; "d",4,5; "e",8,2; "a",5,4] 10) =^ ["c";"e"]);
]

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
