:- module('11', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(lists_ext).
:- use_module(aggregate).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

node('S') --> "S", !.
node('E') --> "E", !.
node(V) -->
  [C],
  { char_type(C, lower),
    char_code(C, V0),
    V is V0 - 0'a }.

line([N|Ns]) --> node(N), line_(Ns).
line_([N|Ns]) --> node(N), !, line_(Ns).
line_([]) --> [].

parse((Width, Tree, Start, End)) -->
  blanks, line(X), parse_(Xs), blanks,
  { length(X, Width),
    flatten([X|Xs], Matrix0),
    get_subst_endpoints(Matrix0, Matrix, Start, End),
    make_tree(Width, Matrix, Tree) }.
parse_([X|Xs]) --> nl, line(X), !, parse_(Xs).
parse_([]) --> [].

index_coord(Width, Index, (X, Y)) :-
  nonvar(Width),
  (   var(Index)
  ->  Index is Y * Width + X
  ;   X is Index mod Width,
      Y is Index div Width
  ).

get_subst_endpoints(_, Matrix, Matrix, Start, End) :-
  nonvar(Start), nonvar(End), !.
get_subst_endpoints(I0, Matrix0, Matrix, Start, End) :-
  nth0(I0, Matrix0, V),
  (   V == 'S'
  ->  Start = I0,
      replace0(I0, Matrix0, 0, Matrix1)
  ;   V == 'E'
  ->  End = I0,
      replace0(I0, Matrix0, 25, Matrix1)
  ;   Matrix1 = Matrix0
  ),
  I is I0 + 1,
  get_subst_endpoints(I, Matrix1, Matrix, Start, End).
get_subst_endpoints(Matrix0, Matrix, Start, End) :-
  get_subst_endpoints(0, Matrix0, Matrix, Start, End).

move(1, 0).
move(-1, 0).
move(0, 1).
move(0, -1).

make_tree(I, _, Matrix, Res, Res) :- length(Matrix, I), !.
make_tree(I0, Width, Matrix, Tree0, Res) :-
  nth0(I0, Matrix, V),
  length(Matrix, Length),
  Height is Length div Width,
  XMax is Width-1,
  YMax is Height-1,
  index_coord(Width, I0, (X0, Y0)),
  findall(I1, (
    move(AX, AY),
    X is X0+AX,
    Y is Y0+AY,
    between(0, XMax, X),
    between(0, YMax, Y),
    index_coord(Width, I1, (X, Y)),
    nth0(I1, Matrix, Nv),
    (V >= Nv; Nv is V + 1)
  ), Node),
  append(Tree0, [Node], Tree),
  I is I0 + 1,
  make_tree(I, Width, Matrix, Tree, Res).

make_tree(Width, Matrix, Res) :- make_tree(0, Width, Matrix, [], Res).

part1((Width, Tree, Start, End), Res) :-
  nonvar(Width), nonvar(Start), nonvar(End), nonvar(Tree),
  var(Res),
  fail.
part2(Data, Res) :- nonvar(Data), var(Res).
