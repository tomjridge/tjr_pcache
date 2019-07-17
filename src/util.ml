type ('a) profile_m = {
  profile_m: 'b. string -> ('b, 'a) m -> ('b, 'a) m
}

let profile_m ~monad_ops ~mark = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let profile_m = 
    if profiling_enabled then (
      fun s m -> 
        return () >>= fun () -> 
        mark s;
        m >>= fun r ->
        mark (s^"'");
        return r)
    else (fun _s m -> m)
  in
  {profile_m}

