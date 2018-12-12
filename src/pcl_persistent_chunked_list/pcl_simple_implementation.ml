(** A simple implementation of pcl *)

type 'e pcl_state = {
  es:'e list
}

let make_pcl_state_ops ~too_large = Pcl_types.{
    nil=(fun () -> {es=[]});
    snoc=(fun i e -> 
      let es' = i.es@[e] in
      match too_large es' with
      | true -> `Error_too_large
      | false -> `Ok {es=es'});
    pl_data=(fun i -> i.es);
}
