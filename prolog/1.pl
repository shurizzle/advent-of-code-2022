:- [prelude].

input_file("inputs/1.txt").

% tests {{{
tinput("").

test1(Data) :- part1(Data, Res), print1(Res), nl.
test1_from_string(Input) :- input_from_string(Input, Data), test1(Data).
test1 :- tinput(Data), test1_from_string(Data).

test2(Data) :- part2(Data, Res), print2(Res), nl.
test2_from_string(Input) :- input_from_string(Input, Data), test2(Data).
test2 :- tinput(Data), test2_from_string(Data).

test(Data) :- test1(Data), test2(Data).
test_from_string(Input) :- input_from_string(Input, Data), test(Data).
test :- tinput(Input), test_from_string(Input).
% }}}

print1(Res) :- print(Res).
print2(Res) :- print(Res).

parse(Es) -->
  blanks, list_of(E, elf(E), blanks_to_nl, UEs), blanks,
  { list_to_ord_set(UEs, Es) }.
elf(E) -->
  list_of(I, raw_line(integer(I)), Cs),
  { length(Cs, L), L > 0,
    sum_list(Cs, E) }.

part1(Data, Res) :- last(Data, Res).
part2(Data, Res) :-
  length(Top3, 3),
  once(append(_, Top3, Data)),
  sum_list(Top3, Res).
