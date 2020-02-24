(** Pcache example, k and v are ints *)

module Make = Pvt_make

module Int_int_ex = Pvt_make.Make_1(Pvt_int_int.Int_int_mrshl)

type blk_id = Blk_id_as_int.blk_id

let make ~blk_alloc ~with_dmap ~write_to_disk 
  : Int_int_ex.dmap_ops 
  = Int_int_ex.make ~blk_alloc ~with_dmap ~write_to_disk 

