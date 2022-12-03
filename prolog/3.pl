:- [prelude].

input_file("inputs/3.txt").

% tests {{{
tinput("vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw").

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

parse(Lines) --> blanks, list_of(L, raw_line(line(L)), Lines), blanks.
line(Is) --> list_of(I, character(I), Is), { length(Is, L), L > 0 }.

character(C) --> [C0], { between(0'a, 0'z, C0), C is C0 - 0'a + 1 }, !.
character(C) --> [C0], { between(0'A, 0'Z, C0), C is C0 - 0'A + 27 }, !.

common_element(Xs, I) :-
  length(Xs, L0),
  L is L0/2,
  length(A, L),
  length(B, L),
  append(A, B, Xs),
  member(I, A),
  member(I, B), !.

groups_of_three([A,B,C|_], [A,B,C]).
groups_of_three([_,_,_|T], G) :-
  length(T, L), L > 0,
  groups_of_three(T, G).

part1(Data, Res) :-
  foldl([Xs, Acc0, Acc]>>(
    common_element(Xs, I),
    Acc is Acc0 + I
  ), Data, 0, Res).
part2(Data, Res) :-
  aggregate_all(
    sum(X),
    (
      groups_of_three(Data, [A,B,C]),
      once((
        member(X, A),
        member(X, B),
        member(X, C)
      ))
    ),
    Res
  ).
