(lang dune 2.7)
(name ssp_verification)
(version 1.0.0)
(generate_opam_files true)

(source (github zajer/trs-ssp-verif))
(license MIT)
(authors "Piotr Cybulski")
(maintainers "pikus3@list.pl")
(homepage "https://github.com/zajer/trs-ssp-verif")
(bug_reports "https://github.com/zajer/trs-ssp-verif")

(package
  (name ssp_verification)
  (synopsis "A library for verification of system modeled as tracking reactive systems.")
  (description 
    "This library allows to verify whether the system was designed correctly as well construct consequetive states of a scenario.
    It is meant to be used with libraries tracking_bigraph,ssp and ssp_bridge.
    On top of that, a module is provided for exporting verification results into structures acceptable by visjs library.
    "
  )
  (tags ("bigraphs" "tracking" "verification" "behavior" "policy"))
  (depends
    (dune (and :build (>= 2.0.0)))
    bigraph
    tracking_bigraph
    csv
    ssp
    ssp_bridge
    (ounit2 :with-test)
  )
)
  
