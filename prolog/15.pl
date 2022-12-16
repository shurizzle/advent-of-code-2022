:- module('13', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(lists_ext).
:- use_module(aggregate).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(I) --> digit(D), integer_(Ds), { number_chars(I, [D|Ds]) }.
integer_([D|Ds]) --> digit(D), !, integer_(Ds).
integer_([]) --> [].

sign_integer(I) --> "-", !, integer(I0), { I is -I0 }.
sign_integer(I) --> "+", !, integer(I).
sign_integer(I) --> integer(I).

line(((#X1, #Y1), (#X2, #Y2))) -->
  "Sensor at x=", sign_integer(X1),
  ", y=", sign_integer(Y1),
  ": closest beacon is at x=", sign_integer(X2),
  ", y=", sign_integer(Y2).

parse([P|Ps]) --> blanks, line(P), parse_(Ps), blanks.
parse_([P|Ps]) --> nl, line(P), !, parse_(Ps).
parse_([]) --> [].

diff(#A, #B, #D) :- #D #= abs(#A - #B).

manhattan((#X1, #Y1), (#X2, #Y2), #D) :-
  diff(#X1, #X2, #X),
  diff(#Y1, #Y2, #Y),
  #D #= #X + #Y.

domain_join(X, Acc0, Acc0..X).
domain_union(D1, D2, D1\/D2).

solve1(Data, #Y, #Res) :-
  findall(Bounds, (
    member(((#X1, #Y1), (#X2, #Y2)), Data),
    manhattan((#X1, #Y1), (#X2, #Y2), #Dist),

    findall(X, (
      manhattan((#X1, #Y1), (#X, #Y), #Dist),
      indomain(X)
    ), Bounds0),
    foldl1(domain_join, Bounds0, Bounds)

  ), Res0),
  foldl1(domain_union, Res0, Res1),
  X in Res1,
  fd_size(X, Res2),
  Res is Res2 - 1.


part1(Data, Res) :- solve1(Data, #20, #Res).
part2(Data, Res) :- nonvar(Data), var(Res).
