:- module('3', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(lambda)).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

character(C) -->
  [C0],
  { char_code(C0, C1),
    between(0'a, 0'z, C1),
    C is C1 - 0'a + 1 }, !.
character(C) -->
  [C0],
  { char_code(C0, C1),
    between(0'A, 0'Z, C1),
    C is C1 - 0'A + 27 }, !.

parse([L|Ls]) --> blanks, line(L), parse_(Ls), blanks.
parse_([L|Ls]) --> nl, line(L), !, parse_(Ls).
parse_([]) --> [].

line([I|Is]) --> character(I), !, line_(Is).
line_([I|Is]) --> character(I), !, line_(Is).
line_([]) --> [].

common_element(Xs, I) :-
  length(Xs, L0),
  L is L0 div 2,
  length(A, L),
  length(B, L),
  append(A, B, Xs),
  member(I, A),
  member(I, B), !.

chunks(Xs, Len, Res) :-
  length(Xs, L),
  L > 0,
  (   L > Len
  ->  length(Res, Len),
      once(append(Res, _, Xs))
  ;   Res = Xs
  ).
chunks(Xs, Len, Res) :-
  length(Xs, L),
  L > Len,
  length(T0, Len),
  once(append(T0, Tail, Xs)),
  chunks(Tail, Len, Res).

part1(Data, Res) :-
  foldl(\Xs^Acc0^Acc^(
    common_element(Xs, I),
    Acc is Acc0 + I
  ), Data, 0, Res).
part2(Data, Res) :-
  findall(X,
    ( chunks(Data, 3, [A,B,C]),
      once((
        member(X, A),
        member(X, B),
        member(X, C)
      )) ),
    Xs),
    sum_list(Xs, Res).
