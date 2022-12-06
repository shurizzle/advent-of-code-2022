:- module('5', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(pairs)).
:- use_module(library(assoc)).
:- use_module(library(lambda)).
:- use_module(lists_ext).

white --> [W], { char_type(W, whitespace), W \== '\n' }.
whites --> white, !, whites.
whites --> [].

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

eol, "\n" --> "\n".
eos([], []).

eol_or_eos --> eol, !.
eol_or_eos --> eos.

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(I) --> digit(D), integer_(Ds), { number_chars(I, [D|Ds]) }.
integer_([D|Ds]) --> digit(D), !, integer_(Ds).
integer_([]) --> [].

crate(nil) --> "   ", !.
crate(C) -->
  "[", [C], "]",
  { char_code(C, C0),
    between(0'A, 0'Z, C0) }.

crates_line([L|Ls]) --> crate(L), crates_line_(Ls).
crates_line_([L|Ls]) --> " ", crate(L), !, crates_line_(Ls).
crates_line_([]) --> whites, eol_or_eos.

cargo(Ls) -->
  crates_line(L), cargo_(Ls0),
  { transpose([L|Ls0], Ls2),
    maplist(exclude(=(nil)), Ls2, Ls) }.
cargo_([L|Ls]) --> nl, crates_line(L), !, cargo_(Ls).
cargo_([]) --> [].

name(I) --> " ", integer(I), " ".

names([I|Is]) --> name(I), names_(Is), whites, eol_or_eos.
names_([I|Is]) --> " ", name(I), !, names_(Is).
names_([]) --> [].

rule((A * C1 -> C2)) -->
  "move ", integer(A),
  " from ", integer(C1),
  " to ", integer(C2),
  whites, eol_or_eos.

rules([R|Rs]) --> rule(R), rules_(Rs).
rules_([R|Rs]) --> nl, rule(R), !, rules_(Rs).
rules_([]) --> [].

parse((Cargo -> Rules)) -->
  cargo(Cargo0), nl, names(Names), !,
  { once(pairs_keys_values(Cargo1, Names, Cargo0)),
    list_to_assoc(Cargo1, Cargo) },
  nl, nl, rules(Rules), blanks.

first_from_all(Cargo, Res) :-
  assoc_to_values(Cargo, Vs),
  maplist(nth0(0), Vs, Res).

apply(Cargo, Rules, Move, Res) :-
  foldl(
    Move+\(Times * L1 -> L2)^Cargo0^O^call(Move, Cargo0, Times, L1, L2, O),
    Rules, Cargo, Cargo1
  ),
  first_from_all(Cargo1, Res0),
  atom_chars(Res, Res0).

move1(Res, 0, _, _, Res) :- !.
move1(Cargo0, N0, L1, L2, Res) :-
  get_assoc(L1, Cargo0, [Crate|NewL1]),
  get_assoc(L2, Cargo0, NewL2_),
  NewL2 = [Crate|NewL2_],

  put_assoc(L1, Cargo0, NewL1, Cargo1),
  put_assoc(L2, Cargo1, NewL2, Cargo),

  N is N0-1,

  move1(Cargo, N, L1, L2, Res).

move2(Cargo0, N, L1, L2, Res) :-
  get_assoc(L1, Cargo0, OldL1),
  get_assoc(L2, Cargo0, OldL2),

  length(Buf, N),
  once(append(Buf, NewL1, OldL1)),
  append(Buf, OldL2, NewL2),

  put_assoc(L1, Cargo0, NewL1, Cargo1),
  put_assoc(L2, Cargo1, NewL2, Res).

part1((Cargo -> Rules), Res) :- apply(Cargo, Rules, move1, Res).
part2((Cargo -> Rules), Res) :- apply(Cargo, Rules, move2, Res).
