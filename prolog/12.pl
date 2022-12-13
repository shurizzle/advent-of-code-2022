:- module('12', [parse//1, solution/3]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(ordsets)).
:- use_module(library(assoc)).
:- use_module(library(dif)).
:- use_module(lists_ext).
:- use_module(aggregate).

% Parse {{{
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

parse((Width, Matrix, Start, End)) -->
  blanks, line(X), parse_(Xs), blanks,
  { length(X, Width),
    flatten([X|Xs], Matrix0),
    get_subst_endpoints(Matrix0, Matrix, Start, End) }.
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
% }}}

% tree {{{
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
% }}}

% astar {{{
astar_current_(FScore, K, Res) :-
  (   get_assoc(K, FScore, Res) -> true
  ;   Res = 0
  ).

astar_current(Res, _, _, [], Res) :- !.
astar_current(I0, V0, FScore, [I1|Open], Res) :-
  astar_current_(FScore, I1, V1),
  (   V1 < V0
  ->  astar_current(I1, V1, FScore, Open, Res)
  ;   astar_current(I0, V0, FScore, Open, Res)
  ).

astar_current(_, [], _) :- !, fail.
astar_current(FScore, [I|Open], Res) :-
  astar_current_(FScore, I, V),
  astar_current(I, V, FScore, Open, Res).

astar_step((_, _, Goal, Heuristic), Current, Neighbor, (Open0, Roots0, GScore0, FScore0), State) :-
  (   get_assoc(Current, GScore0, Tentative0) -> true
  ;   Tentative0 = 0),
  Tentative is Tentative0 + 1,
  (   (   get_assoc(Neighbor, GScore0, NScore)
      ->  Tentative < NScore
      ;   true
      )
  ->  put_assoc(Neighbor, Roots0, Current, Roots),
      put_assoc(Neighbor, GScore0, Tentative, GScore),
      call(Heuristic, Neighbor, Goal, F0),
      F is Tentative + F0,
      put_assoc(Neighbor, FScore0, F, FScore),
      ord_add_element(Open0, Neighbor, Open),
      State = (Open, Roots, GScore, FScore)
  ;   State = (Open0, Roots0, GScore0, FScore0)
  ).

astar_reconstruct(Roots, Parent, Acc, Res) :-
  (   get_assoc(Parent, Roots, Current)
  ->  astar_reconstruct(Roots, Current, [Current|Acc], Res)
  ;   Res = Acc
  ).

astar_reconstruct(Roots, Current, Res) :-
  astar_reconstruct(Roots, Current, [Current], Res).

astar_(GState, (Open0, Roots, GScore, FScore), Res) :-
  dif(Open0, []),
  (Tree, _, Goal, _) = GState,
  astar_current(FScore, Open0, Current),
  (   Current == Goal
  ->  astar_reconstruct(Roots, Current, Res)
  ;   ord_del_element(Open0, Current, Open1),
      nth0(Current, Tree, Neighbors),
      foldl(astar_step(GState, Current), Neighbors, (Open1, Roots, GScore, FScore), State),
      astar_(GState, State, Res)
  ).

astar(Tree, Start, Goal, Heuristic, Path) :-
  Open = [Start],
  empty_assoc(Roots),
  list_to_assoc([Start-0], GScore),
  call(Heuristic, Start, Goal, D),
  list_to_assoc([Start-D], FScore),

  astar_((Tree, Start, Goal, Heuristic), (Open, Roots, GScore, FScore), Path).
% }}}

point_distance((X1, Y1), (X2, Y2), Distance) :-
  Distance is sqrt((X1-X2)^2 + (Y1-Y2)^2).

heuristic(Width, I1, I2, X) :-
  Y1 is I1 div Width,
  X1 is I1 mod Width,
  Y2 is I2 div Width,
  X2 is I2 mod Width,
  point_distance((X1, Y1), (X2, Y2), X).

solve(T, S, G, H, Res) :-
  astar(T, S, G, H, Path),
  length(Path, Res0),
  Res is Res0 - 1.

solution((Width, Matrix, Start, Goal), One, Two) :-
  make_tree(Width, Matrix, Tree),
  solve(Tree, Start, Goal, heuristic(Width), One),
  length(Matrix, Len0),
  Len is Len0 - 1,
  minall(Res, (
    between(0, Len, Start0),
    nth0(Start0, Matrix, 0),
    dif(Start, Start0),
    solve(Tree, Start0, Goal, heuristic(Width), Res)
  ), Two0),
  Two is min(One, Two0).
