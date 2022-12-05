#!/usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"
exec scryer-prolog -g "use_module(library(pio))" -g "use_module(library(format))" -g "use_module(prolog/'1')" -g "phrase_from_file(parse(Data), \"inputs/$1.txt\"), part1(Data, One), format(\"Solution 1: ~w~n\", [One]), part2(Data, Two), format(\"Solution 2: ~w~n\", [Two])" -g halt
