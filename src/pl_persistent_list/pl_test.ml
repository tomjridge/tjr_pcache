open Tjr_monad.Types
open Tjr_monad.State_passing
open Tjr_monad.Mref
open Persistent_list

(* the state of the whole system; 'a is the type of  *)
type ('ptr,'a) state = {
  map: ('ptr*('ptr,'a)list_node) list;  (* association list *)
  cursor_state: ('ptr,'a) plist_state;
  free: int;  (* iso to ptr *)
}

let monad_ops : ('ptr,'a) state state_passing monad_ops = 
  Tjr_monad.State_passing.monad_ops ()

let ( >>= ) = monad_ops.bind 
let return = monad_ops.return

let with_world = Tjr_monad.State_passing.with_world

let read_node ptr s = List.assoc ptr s.map


let write_node ptr n = with_world (fun s -> 
    { s with map=(ptr,n)::s.map } |> fun s ->
    ((),s))

let _ = write_node

(* let tap = ref [] *)

  (*
  let read_node ptr = fun s ->
    (* Printf.printf "ptr is: %d\n" ptr; *)
    (* tap:=s.map; *)
    (s, Ok (List.assoc ptr s.map))  (* ASSUMES ptr in map *)

  let _ = read_node

  (* don't really need it to be in the monad *)
  let read_node ptr = fun s ->
    (* Printf.printf "ptr is: %d\n" ptr; *)
    (* tap:=s.map; *)
    (List.assoc ptr s.map)  (* ASSUMES ptr in map *)
     *)


let read_state () = with_world (fun s -> (s.cursor_state,s))

let _ = read_state

let write_state cursor_state = with_world (fun s ->
    { s with cursor_state } |> fun s ->
    ((),s))

let _ = write_state


let plist_state_ref = {
  get=read_state;
  set=write_state;
}


let alloc ~int_to_ptr = fun () -> with_world (fun s -> 
    ((s.free |> int_to_ptr),{s with free=s.free+1}))


let _ = alloc


let int_to_ptr x = x 

(* NOTE eta expansion *)
let alloc () = alloc ~int_to_ptr ()

let _ = alloc


(* FIXME note eta expansion; can we avoid? *)
let ops () = make_persistent_list ~monad_ops ~write_node ~plist_state_ref ~alloc

let _ = ops


let start_block = 0

let init_state = 
  let root = start_block in
  let current_node={ next=None; contents="Start" } in
  {    
    map=[(root,current_node)]; 
    cursor_state={ current_ptr=root; current_node };
    free=root+1
  }




(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  let get_state () = with_world (fun s -> (s,s)) in
  let ops = ops () in
  let cmds = 
    ops.replace_last "New start" >>= fun () ->
    ops.new_node "second node" >>= fun _ ->
    ops.new_node "third node" >>= fun _ ->
    ops.replace_last "alternative third node" >>= fun () ->
    get_state () >>= fun s ->
    return (plist_to_list ~read_node ~ptr:0 s)
  in
  Tjr_monad.State_passing.run ~init_state cmds |> fun (xs,s) ->
  assert(xs = ["New start";"second node";"alternative third node"]);
  xs |> Tjr_string.concat_strings ~sep:";" |> fun str ->
  print_endline str;
  Printf.printf "%s: ...tests finished\n" __FILE__;
  s  (* actually return the state *)
