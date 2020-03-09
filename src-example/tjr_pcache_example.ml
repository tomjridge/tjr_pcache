(** Pcache example, k and v are ints *)

module Make_1 = Make_1

module Int_int_ex = Make_1.Make(Pvt_int_int.Int_int_mrshl)

type blk_id = Blk_id_as_int.blk_id

let make ~blk_alloc ~with_pcache ~flush_tl 
  : Int_int_ex.pcache_ops 
  = Int_int_ex.make ~blk_alloc ~with_pcache ~flush_tl 

