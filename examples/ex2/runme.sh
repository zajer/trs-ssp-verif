#!/bin/bash

mkdir -p exmp_ex2_content
dune exec --root=../.. ./bin/verify_visjs.exe config.json states.csv trans_normed.csv walk.csv 2