(* testing utilities [do not change] *)

exception SyncDeadlocked
module Event = struct
  include Event

  let tsync t e =
    let timer = new_channel () in
    let run_timer () =
      Thread.delay t;
      poll (send timer None)
    in
    let _ = Thread.create run_timer () in
    match (select [wrap e (fun x -> Some x); receive timer]) with
    | Some x -> x
    | None -> raise SyncDeadlocked

  let tselect t es =
    tsync t (choose es)

  let sync e = tsync 2. e
  let select es = tselect 2. es
end

module Thread = struct
  include Thread

  let tc = ref 0

  let create f a =
    tc := !tc + 1;
    create f a
end


(*****************************************************************************)
(*************************** START OF HOMEWORK *******************************)
(*****************************************************************************)
open Thread
open Event

(* 13.4 *)
let par_unary f = 
  let run_thread e ch = Thread.create (fun _ -> sync (send ch (f e))) () in
  let f' l =
    let channels = List.init (List.length l) (fun _ -> Event.new_channel ()) in
    let threads = List.map2 run_thread l channels in
    let res = List.map (fun ch -> sync (receive ch)) channels 
              |> List.fold_left (fun a x -> a @ [x]) [] in
    List.iter Thread.join threads;
    res
  in
  f'

let map3 f a b c = 
  let rec map3a f a b c acc = match a with  
    | [] -> acc
    | x::xs -> f x (List.hd b) (List.hd c) :: map3a f xs (List.tl b) (List.tl c) acc in
  map3a f a b c []

let par_binary f = 
  let run_thread x y ch = Thread.create (fun _ -> (sync (send ch (f x y)))) () in
  let f' a b = 
    let channels = List.init (List.length a) (fun _ -> Event.new_channel ()) in
    let threads = map3 run_thread a b channels in
    let res = List.map (fun ch -> sync (receive ch)) channels 
              |> List.fold_left (fun a x -> a @ [x]) []  in
    List.iter Thread.join threads;
    res
  in
  f'

(* 13.5 *)
exception OutOfBounds

module Array = struct
  type 'a query = Get of int 
                | Set of (int * 'a) 
                | Resize of (int * 'a)
                | Size
                | Stop
  type 'a result = Some of 'a
                 | Mag of int 
                 | None
                 | Ok
  type 'a t = (('a query) channel * ('a result) channel)

  let chop l len = 
    let rec c l len acc = match l with 
        [] -> acc
      | x::xs -> if len > 1 then c xs (len-1) (x::acc) else c [] (len-1) acc in
    let rec rev l a = match l with 
        [] -> a
      | x::xs -> rev xs (x::a) in
    rev (c l len []) []

  let rec master  len default input output =
    let l =  List.init len (fun _ -> default) in
    let rec run list input output = match sync @@ receive input with
      | Get(i) -> let res = match List.find_opt (fun x -> x = i) list with Some(x) -> Some(x) | None -> None in
        sync @@ send output res;
        run list input output
      | Set(i, x) -> let res = if i >= List.length list then None else Ok in
        sync @@ send output res;
        run (List.mapi (fun j y -> if i = j then x else y) list) input output
      | Resize(x, d) -> let list = if x > List.length list
                          then list @ (List.init ((List.length list) - x) (fun _ -> d))
                          else chop list x in
        sync @@ send output (Ok);
        run list input output
      | Size -> sync @@ send output (Mag(List.length list)); 
        run list input output
      | Stop -> () in
    run l input output

  let fst (a, b) = a
  let snd (a, b) = b

  let post r s = sync @@ send (fst s) r; sync @@ receive (snd s)


  let make n a =
    let input = Event.new_channel () in let output = Event.new_channel () in
    Thread.create (fun _ -> master n a input output) ();
    (input, output)


  let size a = match post Size a with 
    | Mag(n) -> n
    | _ -> raise OutOfBounds

  let set i v a = match post (Set(i, v)) a with 
    | None -> raise OutOfBounds
    | _ -> ()

  let get i a = match post (Get(i)) a with
    | Some(x) -> x
    | _ -> raise OutOfBounds

  let resize n v a = post (Resize (n, v)) a; ()

  let destroy a = post Stop a; ()

end


(* 13.6 *)
exception InvalidOperation
type op = Authenticate of string * string 
        | CreateAccount of string * string
        | CreateDocument of string * string * string
        | View of string * string * int 
        | AddViewer of string * string * int * string
        | ChangeOwner of string * string * int * string
type res = Ok 
         | DocumentID of int
         | Document of string
         | Error
type t = (op channel * res channel)
type user = { name: string; password: string;}
type document = {id: int; content: string; owner: string; viewers: string list}


let fst (a, b) = a
let snd (a, b) = b

let rec run_1 input output doc users id = match sync @@ receive input with
  | CreateAccount(u, p) -> let res = if List.exists (fun x -> x.name = u) users then Error else Ok in
    sync @@ send output res;
    run_1 input output doc (if res = Ok then {name = u; password = p}::users else users) id
  | Authenticate(u, p) -> let res = (match List.find_opt (fun x -> x.name = u) users with
      | Some(x) -> if x.name = u && x.password = p then Ok else Error
      | None -> Error) in
    (* Printf.printf "%s %s %b\n" u p (if res = Error then true else false); *)
    sync @@ send output res;
    run_1 input output doc users id
  | CreateDocument(u, p, d) -> 
    sync @@ send output (DocumentID id);
    run_1 input output ({id = id; content = d; owner= u; viewers = []}::doc) users (id + 1)
  | View(u, p, did) -> let res = (match List.find_opt (fun x -> x.id = did) doc with
      | Some(x) -> if x.owner = u || List.exists (fun x -> x = u) x.viewers 
        then Document x.content else Error
      | None -> Error) in
    sync @@ send output res;
    run_1 input output doc users id
  | AddViewer(u, p, did, viewer) -> let res = match List.find_opt (fun x -> x.id = did) doc with
      | Some(x) -> if x.owner = u then Ok else Error
      | None -> Error in
    let doc = if res = Ok 
      then List.map (fun x -> if x.id = did 
                      then {id = x.id; content = x.content; owner = x.owner; viewers = viewer::x.viewers} 
                      else x) doc
      else doc in
    sync @@ send output res;
    run_1 input output doc users id
  | ChangeOwner(u, p, did, owner) -> let res = match List.find_opt (fun x -> x.id = did) doc with
      | Some(x) -> if x.owner = u then Ok else Error
      | None -> Error in
    let doc = if res = Ok 
      then List.map (fun x -> if x.id = did 
                      then {id = x.id; content = x.content; owner = owner; viewers = x.viewers} 
                      else x) doc
      else doc in
    sync @@ send output res;
    run_1 input output doc users id

let (=>) a b = match a with 
  | Error -> raise InvalidOperation
  | _ -> b ()
let master input output = 
  run_1 input output [] [] 0
let document_server () = 
  let input = Event.new_channel () in let output = Event.new_channel () in
  Thread.create (fun _ -> master input output) (); 
  input, output

let post r s : res = sync @@ send (fst s) r; sync @@ receive (snd s)

let add_account u p s = post (CreateAccount(u, p)) s => fun () -> ()

let authenticate u p s = post (Authenticate(u, p)) s

let publish u p doc s = match authenticate u p s => fun () -> post (CreateDocument(u, p, doc)) s with
  | DocumentID(id) -> id
  | _ -> raise InvalidOperation

let view u p id s = match authenticate u p s => fun () -> post (View(u, p, id)) s with
  | Document(c) -> c
  | _ -> raise InvalidOperation

let add_viewer u p id viewer s = authenticate u p s => fun () -> post (AddViewer(u, p, id, viewer)) s => fun () -> ()
let change_owner u p id owner s = authenticate u p s => fun () -> post (ChangeOwner(u, p, id, owner)) s => fun () -> ()

(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let reset () =
  Thread.tc := 0
let threads_created () =
  !Thread.tc

let d_server () =
  let s = document_server () in
  add_account "user1" "pass1" s;
  add_account "user2" "pass2" s;
  add_account "user3" "pass3" s;
  s

let tests = [
  (* 13.4 *)
  __LINE_OF__ (fun () -> 
      let pinc = par_unary (fun x -> x + 1) in 
      pinc [8;1;1] = [9;2;2] 
      && threads_created () = 3);
  __LINE_OF__ (fun () -> let psof = par_unary string_of_float in psof [7.;1.] = ["7.";"1."] && threads_created () = 2);
  __LINE_OF__ (fun () -> let pmul = par_binary ( * ) in pmul [1;2;3] [5;6;2] = [5;12;6] && threads_created () = 3);
  __LINE_OF__ (fun () -> let pcon = par_binary ( ^ ) in pcon ["th";"";"ver";"nic"] ["is";"is";"y";"e"] = ["this";"is";"very";"nice"] && threads_created () = 4);
  (* 13.5
     NOTE: Array's functions cannot be tested in isolation, so if a test for size fails it may very well be due to a mistake in your implementation of make *)
  (* __LINE_OF__ (fun () -> let _ = Array.make 3 "abc" in threads_created () = 1); *)
  (* __LINE_OF__ (fun () -> let a = Array.make 3 1. in Array.destroy a; threads_created () = 1); *)
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.size a = 3);
  (* __LINE_OF__ (fun () -> let a = Array.make 3 'x' in Array.get 0 a = 'x');
     __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try let _ = Array.get 3 a in false with OutOfBounds -> true); *)
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.set 1 5 a; Array.get 0 a = 0 && Array.get 1 a = 5 && Array.get 2 a = 0 && threads_created () = 1);
  (* __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try Array.set 3 'u' a; false with OutOfBounds -> true); *)
  (* __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 5 1 a; Array.size a = 5 && Array.get 2 a = 0 && Array.get 3 a = 1 && Array.get 4 a = 1 && threads_created () = 1); *)
  (* __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 1 1 a; Array.size a = 1 && Array.get 0 a = 0 && threads_created () = 1); *)
  (* 13.6
     NOTE: Document server functions cannot be tested in isolation, so if a test for view fails it may very well be due to a mistake in your implementation of document_server *)
  __LINE_OF__ (fun () -> let _ = document_server () in threads_created () = 1); (* basic thread creation *)
  __LINE_OF__ (fun () -> let s = document_server () in add_account "user1" "pass1" s; true); (* add correct account *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_account "user1" "***" s; false with InvalidOperation -> true); (* account exists already *)
  __LINE_OF__ (fun () -> let s = d_server () in publish "user2" "pass2" "My Document" s <> publish "user1" "pass1" "My Document" s); (* publish document *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = publish "user1" "***" "My Document" s in false with InvalidOperation -> true); (* publish incorrect auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = view "user1" "pass1" 0 s in false with InvalidOperation -> true); (* view invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in "text" = view "user1" "pass1" d s); (* view correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try let _ = view "user2" "pass2" d s in false with InvalidOperation -> true); (* view, no access *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_viewer "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* add viewer invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try add_viewer "user1" "***" d "user3" s; false with InvalidOperation -> (try let _ = view "user3" "pass3" d s in false with InvalidOperation -> true)); (* add viewer invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user2" "pass2" "text" s in add_viewer "user2" "pass2" d "user1" s; view "user1" "pass1" d s = "text"); (* add viewer correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user1" "***" d "user2" s; false with InvalidOperation -> true); (* change owner invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try change_owner "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* change owner invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user2" "pass2" d "user2" s; false with InvalidOperation -> true); (* change owner, not owner *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in change_owner "user1" "pass1" d "user3" s; view "user3" "pass3" d s = "mydoc"); (* change owner correct *)
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
    reset ();
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)


