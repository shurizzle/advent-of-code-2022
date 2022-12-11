:- module('11', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(lists_ext).

white --> [W], { char_type(W, whitespace), W \== '\n' }.
whites --> white, !, whites.
whites --> [].

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(I) --> digit(D), integer_(Ds), { number_chars(I, [D|Ds]) }.
integer_([D|Ds]) --> digit(D), !, integer_(Ds).
integer_([]) --> [].

items([I|Is]) --> integer(I), !, items_(Is).
items([]) --> [].
items_([I|Is]) --> whites, ",", whites, integer(I), !, items_(Is).
items_([]) --> [].

items_line(Is) --> "  Starting items:", whites, items(Is).

operand(old) --> "old", !.
operand(I) --> integer(I).

operation(Op) -->
  { operator(O) },
  operand(O1), whites,
  [O], !,
  whites, operand(O2),
  { Op0 =.. [O, O1, O2],
    Op = eval_op(Op0) }.

operation_line(Op) --> "  Operation: new", whites, "=", whites, operation(Op).

test_div(I) --> "  Test: divisible by ", integer(I).

test_if_true(M) --> "    If true: throw to monkey ", integer(M).
test_if_false(M) --> "    If false: throw to monkey ", integer(M).

test_lines(test_div(By, IfTrue, IfFalse)) -->
  test_div(By), nl,
  test_if_true(IfTrue), nl,
  test_if_false(IfFalse).

monkey(-(N, (Is, Op, Test, 0))) -->
  "Monkey ", integer(N), ":", nl,
  items_line(Is), nl,
  operation_line(Op), nl,
  test_lines(Test).

parse(Ms) -->
  blanks, monkey(M), parse_(Ms0), blanks,
  { list_to_assoc([M|Ms0], Ms) }.
parse_([M|Ms]) --> whites, nl, whites, nl, monkey(M), !, parse_(Ms).
parse_([]) --> [].

operator(+).
operator(-).
operator(*).
operator(/).

test_div(By, IfTrue, IfFalse, N, Res) :-
  (   0 =:= N mod By
  ->  Res = IfTrue
  ;   Res = IfFalse
  ).

resolve_op(old, Res, Res).
resolve_op(Res, _, Res) :- integer(Res).
eval_op(Op0, Old, Res) :-
  Op0 =.. [O, O1, O2],
  nonvar(O), operator(O),
  once(resolve_op(O1, Old, OO1)),
  once(resolve_op(O2, Old, OO2)),
  Op =.. [O, OO1, OO2],
  Res is Op.

round(Monkeys, Relief, Res) :-
  assoc_to_keys(Monkeys, Ks),
  round(Ks, Monkeys, Relief, Res).

round([], Res, _, Res) :- !.
round([K|Ks], Monkeys0, Relief, Res) :-
  monkey_round(K, Monkeys0, Relief, Monkeys),
  round(Ks, Monkeys, Relief, Res).

shift_item(K, Monkeys0, (Item, Op, Test), Monkeys) :-
  get_assoc(K, Monkeys0, ([Item|T], Op, Test, Inspect0)),
  Inspect is Inspect0 + 1,
  put_assoc(K, Monkeys0, (T, Op, Test, Inspect), Monkeys).

unshift_item(K, Monkeys0, Item, Monkeys) :-
  get_assoc(K, Monkeys0, (Items0, Op, Test, Inspect)),
  append(Items0, [Item], Items),
  put_assoc(K, Monkeys0, (Items, Op, Test, Inspect), Monkeys).

monkey_round(K, Monkeys0, Relief, Res) :-
  (   shift_item(K, Monkeys0, (Item, Op, Test), Monkeys1)
  ->  monkey_round_(Monkeys1, Relief, Item, Op, Test, Monkeys),
      monkey_round(K, Monkeys, Relief, Res)
  ;   Res = Monkeys0
  ).

monkey_round_(Monkeys0, Relief, Item0, Op, Test, Res) :-
  call(Op, Item0, Item1),
  call(Relief, Item1, Item),
  call(Test, Item, K),
  unshift_item(K, Monkeys0, Item, Res).

monkey_inspect((_, _, _, Res), Res).

gcd(X, 0, X) :- !.
gcd(X, Y, Z) :-
  H is X rem Y,
  gcd(Y, H, Z).
lcm(X,Y,LCM):-
  gcd(X,Y,GCD),
  LCM is X*Y//GCD.

monkey_div((_, _, test_div(Res, _, _), _), Res).

monkey_lcm(Monkey, Acc0, Acc) :-
  monkey_div(Monkey, Div),
  lcm(Acc0, Div, Acc).

monkeys_lcm([Monkey|Monkeys], Res) :-
  monkey_div(Monkey, Div),
  foldl(monkey_lcm, Monkeys, Div, Res).

div_by(X, N, Res) :- Res is N // X.
mod_by(X, N, Res) :- Res is N mod X.

rounds(0, Res, _, Res) :- !.
rounds(N0, Monkeys0, Relief, Res) :-
  round(Monkeys0, Relief, Monkeys),
  N is N0 - 1,
  rounds(N, Monkeys, Relief, Res).

solve(Data, Rounds, Relief, Res) :-
  rounds(Rounds, Data, Relief, Res0),
  assoc_to_values(Res0, Res1),
  maplist(monkey_inspect, Res1, Inspects0),
  sort(Inspects0, Inspects),
  length(Last2, 2),
  once(append(_, Last2, Inspects)),
  mul_list(Last2, Res).

part1(Data, Res) :- solve(Data, 20, div_by(3), Res).
part2(Data, Res) :-
  assoc_to_values(Data, Monkeys),
  monkeys_lcm(Monkeys, Lcm),
  solve(Data, 10000, mod_by(Lcm), Res).
