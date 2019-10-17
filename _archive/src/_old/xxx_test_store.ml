let set,get = Tjr_store.(set,get)

let test_store = 
  Tjr_fs_shared.Global.register
    ~name:"pl_test test_store ref"
    (ref Tjr_store.initial_store)

let mk_ref' v = 
  !test_store |> fun t ->
  Tjr_store.mk_ref v t |> fun (t,r) ->
  test_store:=t;
  r
