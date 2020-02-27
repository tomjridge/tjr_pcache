[%%import "pcache_optcomp_config.ml"]

module Internal : sig 
  val pl_profiler : int Tjr_profile.profiler 
  val pcl_profiler : int Tjr_profile.profiler 
  val dcl_profiler : int Tjr_profile.profiler 
  val pcache_profiler : int Tjr_profile.profiler 
end = struct

[%%if PROFILE_PL]
let pl_profiler = make_profiler ~print_header:"pl_profiler" ()
[%%else]
let pl_profiler = Tjr_profile.dummy_profiler
[%%endif]

[%%if PROFILE_PCL]
let pcl_profiler = make_profiler ~print_header:"pcl_profiler" ()
[%%else]
let pcl_profiler = Tjr_profile.dummy_profiler
[%%endif]


[%%if PROFILE_DCL]
let dcl_profiler = make_profiler ~print_header:"dcl_profiler" ()
[%%else]
let dcl_profiler = Tjr_profile.dummy_profiler
[%%endif]


[%%if PROFILE_DMAP]
let pcache_profiler = make_profiler ~print_header:"pcache_profiler" ()
[%%else]
let pcache_profiler = Tjr_profile.dummy_profiler
[%%endif]


end

include Internal
