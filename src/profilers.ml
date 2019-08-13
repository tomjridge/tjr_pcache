[%%import "pcache_optcomp_config.ml"]

module Internal : sig 
  val pl_profiler : int Tjr_profile.profiler 
  val pcl_profiler : int Tjr_profile.profiler 
  val dcl_profiler : int Tjr_profile.profiler 
  val dmap_profiler : int Tjr_profile.profiler 
end = struct

[%%if PROFILE_PL]
let pl_profiler = make_profiler ()
[%%else]
let pl_profiler = Tjr_profile.dummy_profiler
[%%endif]

[%%if PROFILE_PCL]
let pcl_profiler = make_profiler ()
[%%else]
let pcl_profiler = Tjr_profile.dummy_profiler
[%%endif]


[%%if PROFILE_DCL]
let dcl_profiler = make_profiler ()
[%%else]
let dcl_profiler = Tjr_profile.dummy_profiler
[%%endif]


[%%if PROFILE_DMAP]
let dmap_profiler = make_profiler ()
[%%else]
let dmap_profiler = Tjr_profile.dummy_profiler
[%%endif]


end

include Internal
