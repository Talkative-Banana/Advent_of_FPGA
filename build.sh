#!/bin/bash
set -e
MODE=${1:-prob1}

ocamlfind ocamlopt -package hardcaml -linkpkg ${MODE}.ml -o ${MODE}.out
./${MODE}.out
