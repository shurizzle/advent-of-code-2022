:- module('10', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(between)).
:- use_module(aggregate).
:- use_module(lists_ext).

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

indexes(20).
indexes(60).
indexes(100).
indexes(140).
indexes(180).
indexes(220).

indexes((I0, X), Acc0, Acc) :-
  I1 is I0+1,
  (   indexes(I1)
  ->  Acc is Acc0+I1*X
  ;   Acc is Acc0
  ).

part1(Data, Res) :-
  foldall(indexes, (I,X), run(Data, I, X), 0, Res).

write_canvas((I0, X), Canvas0, Canvas) :-
  Pos is I0 mod 40,
  (   Start is max(0, X-1),
      Stop is min(X+1, 39),
      between(Start, Stop, Pos)
  ->  replace0(I0, Canvas0, '#', Canvas)
  ;   Canvas = Canvas0
  ).

canvas_line_size(L) :- length(L, 40).

nl_joiner(S2, S1, S) :-
  append([S1, "\n" ,S2], S).

canvas_string(Canvas, String) :-
  length(Lines, 6),
  maplist(canvas_line_size, Lines),
  append(Lines, Canvas),
  foldl1(nl_joiner, Lines, String).

part2(Data, Res) :-
  L is 40*6,
  length(Canvas0, L),
  maplist(=(' '), Canvas0),
  foldall(write_canvas, (I,X), run(Data, I, X), Canvas0, Canvas),
  canvas_string(Canvas, Res0),
  append("\n", Res0, Res).
