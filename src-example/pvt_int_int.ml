module Blk_id = Blk_id_as_int
type blk_id = Blk_id.blk_id [@@deriving bin_io, yojson]

module type MRSHL' = MRSHL with type k = int and type v = int and type r = blk_id

module Int_int_mrshl (* : MRSHL' *) = struct
  open Bin_prot.Std
  type k = int [@@deriving bin_io, yojson]
  type v = int [@@deriving bin_io, yojson]  
  type r = blk_id [@@deriving bin_io, yojson]
  type kvop = (k,v)Kvop.kvop [@@deriving bin_io, yojson]

  (** This is the max # of bytes required for k *)
  let k_size = 9
  let v_size = 9
  let r_size = 9

  let k_cmp: k -> k -> int = Stdlib.compare
end
