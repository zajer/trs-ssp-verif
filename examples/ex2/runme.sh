#!/bin/bash

mkdir -p exmp_2_content
dune exec --root=../.. ./bin/verify_visjs.exe config.json states.csv trans_normed.csv walk.csv 2 exmp_2