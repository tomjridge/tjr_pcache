(* A persistent list of values, implemented as a persistent
   singly-linked list. *)

(*
#thread;;
#require "tjr_btree";;
#require "tjr_lib";;
*)

(* FIXME mref should be in tjr_monad *)
open Tjr_btree.Base_types  (* mref *)
open Tjr_monad.Monad

(* nodes in the list have an optional next pointer, and contents *)
(* FIXME rename to plist_node *)
type ('ptr,'a) list_node = {
  next: 'ptr option;
  contents: 'a;
}

(* the cursor state is in memory; make sure to write the current_node
   to disk; NOTE 'a is the type of the contents of the list_node

*)
type ('ptr,'a) plist_state (* cursor_state *) = {
  current_ptr: 'ptr;  (* block we are currently updating *)
  current_node: ('ptr,'a) list_node;  (* stored in mem to avoid rereading when moving to new node FIXME? *)
}

(* FIXME rename plist *)
type ('a,'ptr,'t) list_ops = {
  replace_last: 'a -> (unit,'t) m;
  new_node: 'a -> ('ptr,'t) m;  (* NOTE we return the ptr to the new node *)
}



let make_persistent_list 
    ~monad_ops
    ~(write_node : 'ptr -> ('ptr,'a) list_node -> (unit,'t) m) 
    ~(plist_state_ref : (('ptr,'a) plist_state,'t) mref)
    ~(alloc : unit -> ('ptr,'t) m)
    : ('a,'ptr,'t) list_ops
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let read_state,write_state = plist_state_ref.get, plist_state_ref.set in
  let replace_last contents = 
    read_state () >>= fun s ->
    let current_node = { s.current_node with contents } in
    write_node s.current_ptr current_node >>= fun () ->
    write_state { s with current_node } >>= fun () -> 
    return ()
  in
  let new_node contents = 
    alloc () >>= fun new_ptr ->
    read_state () >>= fun { current_ptr; current_node } ->
    (* write the current block with a valid next ptr *)
    let next = Some new_ptr in
    {current_node with next } |> write_node current_ptr >>= fun () ->
    (* construct new node *)
    { next=None; contents } |> fun new_node ->
    write_node new_ptr new_node >>= fun () ->
    { current_ptr=new_ptr; current_node=new_node } |> fun s ->
    write_state s >>= fun () ->
    return new_ptr
  in
  { replace_last; new_node }


let _ = make_persistent_list


let plist_to_nodes 
    ~(read_node:'ptr -> 't -> ('ptr,'a)list_node) 
    ~(ptr:'ptr)
    s 
  : ('ptr * ('ptr,'a)list_node) list 
  (* NOTE in the return type, the first ptr is the pointer to the
     node; the snd is the optional link in the list_node *)
  =
  let rec loop ptr = 
    read_node ptr s |> fun node ->
    match node.next with 
    | None -> [(ptr,node)]
    | Some ptr' -> (ptr,node)::(loop ptr')
  in
  loop ptr

let _ = plist_to_nodes


let plist_to_list ~read_node ~ptr s = 
  plist_to_nodes ~read_node ~ptr s |> List.map (fun (_,n) -> n.contents)


(*
let rec plist_to_list ~read_node ~ptr =
  let acc = ref [] in
  let rec loop ptr = 
    read_node ptr >>= fun { next; contents } ->
    acc:=contents::!acc;
    match next with
    | None -> return (List.rev !acc)
    | Some ptr -> loop ptr
  in
  loop ptr



let _ = plist_to_list
*)


(* testing ---------------------------------------------------------- *)

module Test = struct 

  open Tjr_monad
  open Tjr_monad.Monad

  (* the state of the whole system; 'a is the type of  *)
  type ('ptr,'a) state = {
    map: ('ptr*('ptr,'a)list_node) list;  (* association list *)
    cursor_state: ('ptr,'a) plist_state;
    free: int;  (* iso to ptr *)
  }

  let monad_ops : ('ptr,'a) state state_passing monad_ops = 
    Tjr_monad.State_passing_instance.monad_ops ()

  let ( >>= ) = monad_ops.bind 
  let return = monad_ops.return

  let with_world = Tjr_monad.State_passing_instance.with_world

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
    State_passing_instance.run ~init_state cmds |> fun (xs,s) ->
    assert(xs = ["New start";"second node";"alternative third node"]);
    xs |> Tjr_string.concat_strings ~sep:";" |> fun str ->
    print_endline str;
    s  (* actually return the state *)

end
