:- module('6', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nonblank(C) --> [C], { \+ char_type(C, whitespace) }.
nonblanks([C|Cs]) --> nonblank(C), !, nonblanks(Cs).
nonblanks([]) --> [].

parse(Cs) --> blanks, nonblanks(Cs), blanks.

seqs(List, Len, C, Res) :-
  length(List, L),
  L >= Len,
  length(Res0, Len),
  (   once(append(Res0, _, List)),
      Res = C:Res0
  ;   [_|Tail] = List,
      C0 is C+1,
      seqs(Tail, Len, C0, Res)
  ).
seqs(List, Len, Res) :-
  seqs(List, Len, 0, Res).

solve(Data, Len, Res) :-
  seqs(Data, Len, Res0:Slice),
  length(Slice, L),
  list_to_ord_set(Slice, S0),
  length(S0, L),
  Res is Res0+Len, !.

part1(Data, Res) :- solve(Data, 4, Res).
part2(Data, Res) :- solve(Data, 14, Res).
