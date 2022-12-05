:- module('4', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(aggregate).

:- op(900, xfx, ..).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(I) --> digit(D), integer_(Ds), { number_chars(I, [D|Ds]) }.
integer_([D|Ds]) --> digit(D), !, integer_(Ds).
integer_([]) --> [].

range(Start..End) --> integer(Start), "-", integer(End).

line((R1,R2)) --> range(R1), ",", range(R2).

parse([L|Ls]) --> blanks, line(L), !, parse_(Ls), blanks.
parse_([L|Ls]) --> nl, line(L), !, parse_(Ls).
parse_([]) --> [].

:- op(900, xfx, contains).

contains(AStart..AEnd, BStart..BEnd) :-
  AStart =< BStart,
  AEnd >= BEnd.

:- op(900, xfx, overlaps).

overlaps(AStart..AEnd, BStart..BEnd) :-
  AStart =< BEnd,
  BStart =< AEnd.

part1(Data, Res) :-
  countall(1,
          ( member((A,B), Data),
            once((A contains B; B contains A))
          ),
          Res).
part2(Data, Res) :-
  countall(1,
          ( member((A,B), Data),
            A overlaps B
          ),
          Res).
