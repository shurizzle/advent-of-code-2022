:- [prelude].

input_file("inputs/2.txt").

% tests {{{
tinput("A Y
B X
C Z").

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
line(O:M) --> opponent_play(O), white, my_play(M).

opponent_play(rock) --> "A".
opponent_play(paper) --> "B".
opponent_play(scissors) --> "C".

my_play(rock) --> "X".
my_play(paper) --> "Y".
my_play(scissors) --> "Z".

shape_value(rock, 1).
shape_value(paper, 2).
shape_value(scissors, 3).

round_result(rock, rock, draw) :- !.
round_result(rock, paper, win) :- !.
round_result(rock, scissors, loss) :- !.
round_result(paper, rock, loss) :- !.
round_result(paper, paper, draw) :- !.
round_result(paper, scissors, win) :- !.
round_result(scissors, rock, win) :- !.
round_result(scissors, paper, loss) :- !.
round_result(scissors, scissors, draw) :- !.

result_value(draw, 3).
result_value(loss, 0).
result_value(win, 6).

round_outcome(X, Y, Res, Value) :-
  round_result(X, Y, Res),
  shape_value(Y, V0),
  round_result(X, Y, RR),
  result_value(RR, V1),
  Value is V0 + V1.

convert_my_move(rock, loss).
convert_my_move(paper, draw).
convert_my_move(scissors, win).

part1(Data, Res) :- foldl([X:Y, Acc, Out]>>(
    round_outcome(X, Y, _, V),
    Out is Acc + V
  ), Data, 0, Res).
part2(Data, Res) :- foldl([X:Res0, Acc, Out]>>(
    convert_my_move(Res0, Res),
    round_outcome(X, _, Res, V),
    Out is Acc + V
  ), Data, 0, Res).
