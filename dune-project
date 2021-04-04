(lang dune 2.7)
(name ssp_verification)
(version 0.1.0)
(generate_opam_files true)

(source (github zajer/trs-ssp-verif))
(license MIT)
(authors "Piotr Cybulski")
(maintainers "pikus3@list.pl")
(homepage "https://github.com/zajer/trs-ssp-verif")
(bug_reports "https://github.com/zajer/trs-ssp-verif")

(package
  (name ssp_verification)
  (synopsis "")
  (description 
    ""
  )
  (tags ())
  (depends
    (dune (and :build (>= 2.0.0)))
    bigraph
    tracking_bigraph
    csv
    ssp
    (ounit2 :with-test)
  )
)
  
