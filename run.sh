#!/usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"

exec scryer-prolog \
  -g "use_module(library(pio))" \
  -g "use_module(library(format))" \
  -g "use_module(library(diag))" \
  -g "use_module(prolog/'$1')" \
  -g "phrase_from_file(parse(Data), \"inputs/$1.txt\"), (catch(wam_instructions('$1':solution/3, _), _, false) -> solution(Data, One, Two), format(\"Solution 1: ~w~nSolution 2: ~w~n\", [One, Two]); part1(Data, One), format(\"Solution 1: ~w~n\", [One]), part2(Data, Two), format(\"Solution 2: ~w~n\", [Two]))" \
  -g halt
