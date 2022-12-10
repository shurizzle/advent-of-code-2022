:- module('9', [parse//1, part1/2, part2/2
              , test1/1, test2/1]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
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

direction(up) --> "U".
direction(down) --> "D".
direction(left) --> "L".
direction(right) --> "R".

instruction((D * T)) --> direction(D), " ", integer(T).

parse([I|Is]) --> blanks, instruction(I), parse_(Is), blanks.
parse_([I|Is]) --> nl, instruction(I), !, parse_(Is).
parse_([]) --> [].

test_input1("R 4\n\
U 4\n\
L 3\n\
D 1\n\
R 4\n\
D 1\n\
L 5\n\
R 2").
test_input2("R 5\n\
U 8\n\
L 8\n\
D 3\n\
R 17\n\
D 10\n\
L 25\n\
U 20").

test_data1(Data) :- test_input1(I), phrase(parse(Data), I).
test_data2(Data) :- test_input2(I), phrase(parse(Data), I).

test1(Res) :- test_data1(Data), part1(Data, Res).
test2(Res) :- test_data2(Data), part2(Data, Res).

unroll_instructions(Insts, Direction) :-
  member((D * T), Insts),
  unroll_instructions_(D, T, Direction).
unroll_instructions_(_, 0, _) :- !, fail.
unroll_instructions_(D, _, D).
unroll_instructions_(D, T0, Direction) :-
  T is T0-1,
  unroll_instructions_(D, T, Direction).

clamp(Min, Max, V0, V) :-
  V is min(Max, max(Min, V0)).

distance((X1, Y1), (X2, Y2), D) :-
  D is floor(sqrt((X2-X1)^2+(Y2-Y1)^2)).

follow((X1, Y1), (X2, Y2), (X, Y)) :-
      distance((X1, Y1), (X2, Y2), D),
      D > 1
  ->  clamp(-1, 1, X2-X1, X3),
      clamp(-1, 1, Y2-Y1, Y3),
      X is X1+X3,
      Y is Y1+Y3
  ;   X is X1,
      Y is Y1.

move((X, Y0), up,    (X, Y)) :- !, Y is Y0+1.
move((X, Y0), down,  (X, Y)) :- !, Y is Y0-1.
move((X0, Y), left,  (X, Y)) :- !, X is X0-1.
move((X0, Y), right, (X, Y)) :- !, X is X0+1.

evolve([Head0|Knot], Direction, Res) :-
  move(Head0, Direction, Head),
  scanl(follow, Knot, Head, Res).

make_state(P, Knots, (Pos2, Pos10, Points)) :-
  list_to_ord_set([P], Pos2),
  list_to_ord_set([P], Pos10),
  length(Points, Knots),
  maplist(=(P), Points).

initial_state(State) :- make_state((0, 0), 10, State).

step(Direction, (Pos20, Pos100, Points0), (Pos2, Pos10, Points)) :-
  evolve(Points0, Direction, Points),
  nth1(2, Points, P2),
  nth1(10, Points, P10),
  ord_add_element(Pos20, P2, Pos2),
  ord_add_element(Pos100, P10, Pos10).

run(Instructions, (L2, L10)) :-
  initial_state(State0),
  foldall(step, Direction, unroll_instructions(Instructions, Direction), State0, (Pos2, Pos10, _)),
  length(Pos2, L2),
  length(Pos10, L10).

part1(Data, Res) :- run(Data, (Res, _)).
part2(Data, Res) :- run(Data, (_, Res)).
