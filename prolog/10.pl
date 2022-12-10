:- module('9', [parse//1, solution/3, test_parse/1, run/3]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

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

op_(addx(I)) --> "addx ", !, sign_integer(I).
op_(noop) --> "noop".

parse([Op|Ops]) --> blanks, op_(Op), parse_(Ops), blanks.
parse_([Op|Ops]) --> nl, op_(Op), !, parse_(Ops).
parse_([]) --> [].

test_input("noop\n\
addx 3\n\
addx -5").

test_data1(Data) :- test_input(I), phrase(parse(Data), I).
test_data2(Data) :- phrase_from_file(parse(Data), "inputs/10-test.txt").

test_parse(Data) :- test_data1(Data).

noop(_, I, X, I, X).
noop(Ops, I0, X0, I, X) :-
  I1 is I0+1,
  run(Ops, I1, X0, I, X).

addx(_, _, I, X, I, X).
addx(_, _, I0, X, I, X) :- I is I0+1.
addx(A, Ops, I0, X0, I, X) :-
  I1 is I0+2,
  X1 is X0+A,
  run(Ops, I1, X1, I, X).

run([], I, X, I, X).
run([Op|Ops], I0, X0, I, X) :-
  call(Op, Ops, I0, X0, I, X).

run(Ops, I, X) :- run(Ops, 0, 1, I, X).

solution(_,_,_).
