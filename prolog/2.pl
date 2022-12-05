:- module('2', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(lambda)).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

opponent(rock) --> "A".
opponent(paper) --> "B".
opponent(scissors) --> "C".

me(rock) --> "X".
me(paper) --> "Y".
me(scissors) --> "Z".

line(O:M) --> opponent(O), " ", me(M).

parse([O:M|Xs]) --> blanks, line(O:M), !, parse_(Xs), blanks.

parse_([O:M|Xs]) --> nl, line(O:M), !, parse_(Xs).
parse_([]) --> [].

%! round_outcome(?Opponent, ?Me, ?Result, +Value) is det
%! round_outcome(+Opponent, +Me, ?Result, ?Value) is det
%! round_outcome(+Opponent, ?Me, +Result, ?Value) is det
%! round_outcome(?Opponent, +Me, +Result, ?Value) is det
%! round_outcome(?Opponent, ?Me, ?Result, ?Value) is nondet
round_outcome(O, M, R, V) :-
  nonvar(V), !,
  once(round_outcome_(O, M, R, V)).
round_outcome(O, M, R, V) :-
  ( nonvar(O), (nonvar(M); nonvar(R))
  ; nonvar(R), nonvar(M) ), !,
  once(round_outcome_(O, M, R, V)).
round_outcome(O, M, R, V) :-
  round_outcome_(O, M, R, V).

round_outcome_(rock, rock, draw, 4).
round_outcome_(rock, paper, win, 8).
round_outcome_(rock, scissors, loss, 3).
round_outcome_(paper, rock, loss, 1).
round_outcome_(paper, paper, draw, 5).
round_outcome_(paper, scissors, win, 9).
round_outcome_(scissors, rock, win, 7).
round_outcome_(scissors, paper, loss, 2).
round_outcome_(scissors, scissors, draw, 6).

%! remap_me(+Shape, ?Result) is det
%! remap_me(?Shape, +Result) is det
%! remap_me(?Shape, ?Result) is nondet
remap_me(rock, loss).
remap_me(paper, draw).
remap_me(scissors, win).

part1(Data, Res) :-
  foldl(\OM^Acc0^Acc^(
    O:M = OM,
    round_outcome(O, M, _, V),
    Acc is Acc0 + V
  ), Data, 0, Res).
part2(Data, Res) :-
  foldl(\OR^Acc0^Acc^(
    O:Res0 = OR,
    remap_me(Res0, Res),
    round_outcome(O, _, Res, V),
    Acc is Acc0 + V
  ), Data, 0, Res).
