(lang dune 2.0)
(name tjr_pcache)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_pcache))
(homepage             https://github.com/tomjridge/tjr_pcache)
(documentation        https://tomjridge.github.io/ocamldocs/)
(generate_opam_files  true)


(package 
 (name tjr_pcache)
 (synopsis "tjr_pcache: A persistent cache (part of ImpFS)")
 (depends 
bigstring
tjr_plist
ppx_jane
ppx_deriving_yojson

))

;; (package 
;;  (name tjr_pcache_examples)
;;  (synopsis "tjr_pcache: A persistent cache (part of ImpFS) (examples)")
;;  (depends
;; tjr_pcache
;; alcotest
;; core
;; 
;;    ))

;; (package 
;;  (name tjr_pcache_test)
;;  (synopsis "tjr_pcache: A persistent cache (part of ImpFS) (test)")
;;  (depends
;; tjr_pcache
;; alcotest
;; core
;;    ))
