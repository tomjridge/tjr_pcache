set -a # export all vars
# set -x # debug

libname=tjr_pcache
Libname=Tjr_pcache


meta_description="tjr_pcache, a persistent cache (part of ImpFS)."

required_packages="tjr_btree,tjr_lib" 

# FIXME a bit inefficient if recalculating every time
mls=`ocamlfind ocamldep -package $required_packages -sort -one-line *.ml`

natives=""
bytes=""


# common ---------------------------------------------------------------



# set these env vars before including the file
function check_env_vars () {
    # http://stackoverflow.com/questions/31164284/shell-script-exiting-script-if-variable-is-null-or-empty
    : ${libname?Need a value}
    : ${Libname?Need a value}
    : ${mls?Need a value}
    : ${meta_description?Need a value}
    : ${required_packages?Need a value}
}
check_env_vars

PKGS="-package $required_packages"


# root=$(realpath $(dirname $BASH_SOURCE))/../..
# 
#  # if using nix, this may not be present
# test -f $root/config.sh && source $root/config.sh


SYNTAX="" # "-syntax camlp4o" # simplify: use for every file
FLGS="-g -thread -bin-annot" 
INLINE="-inline 0" # inline 0 for debugging native
FOR_PACK="-for-pack $Libname"

    # 8~"pattern-matching is not exhaustive"; 
    # 11~"this match case is unused";
    # 20~argument will not be used FIXME re-enable this
    # 26~"unused variable s2"
    # 40~It is not visible in the current scope, and will not be selected if the type becomes unknown.
WARN="-w @f@p@u@s@40-8-11-26-40-20"

    # these include syntax, so should work on all files; may be
    # overridden in ocamlc.sh; FIXME don't need -bin-annot twice
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -bin-annot         $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -bin-annot $INLINE $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"


# mls ----------------------------------------

cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"


# cma,cmxa -------------------------------------------------------------

function mk_cma() {
         # NOTE -bin-annot
	$DISABLE_BYTE ocamlfind ocamlc -bin-annot -pack -o $libname.cmo $cmos
  $DISABLE_BYTE ocamlfind ocamlc -g -a -o $libname.cma $libname.cmo
}

function mk_cmxa() {
	$DISABLE_NTVE ocamlfind ocamlopt -pack -o $libname.cmx $cmxs
  $DISABLE_NTVE ocamlfind ocamlopt -g -a -o $libname.cmxa $libname.cmx
}




# meta ----------------------------------------

function mk_meta() {
#local gv=`??`
#local d=`??`
cat >META <<EOF
name="$libname"
description="$meta_description"
version="??"
requires="$required_packages"
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF

}



# doc ----------------------------------------------------

function mk_doc() {
    ocamlfind ocamldoc -short-paths $PKGS $WARN -html `cat depend/*`
}


# clean ----------------------------------------------------------------

function clean() {
	rm -f *.{cmi,cmo,cmx,o,cmt} a.out *.cma *.cmxa *.a *.byte *.native
}

# ocamlfind install, remove, reinstall --------------------

function install() {
    # assumes packing
	  ocamlfind install $libname META $libname.{cmi,cmo,cma,cmx,cmxa,a,cmt} *.cmt *.ml
    # FIXME how to install cmt file for libname?
}

function remove() {
    ocamlfind remove $libname
}
