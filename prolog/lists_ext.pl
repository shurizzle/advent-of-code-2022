:- module(lists_ext, [exclude/3, scanl/4, foldl1/3, replace0/4, mul_list/2]).

:- use_module(library(lists)).

:- meta_predicate exclude(1, ?, ?).

exclude(Goal, Ls0, Ls) :-
  exclude_(Ls0, Goal, Ls).

exclude_([], _, []).
exclude_([L|Ls0], Goal, Ls) :-
  (   call(Goal, L) ->
      Ls = Rest
  ;   Ls = [L|Rest]
  ),
  exclude_(Ls0, Goal, Rest).

:- meta_predicate scanl(3, +, +, -).

scanl(Goal, List, V0, [V0|Values]) :-
  scanl_(List, Goal, V0, Values), !.
scanl_([], _, _, []).
scanl_([H|T], Goal, V, [VH|VT]) :-
  call(Goal, H, V, VH),
  scanl_(T, Goal, VH, VT).

:- meta_predicate foldl1(3, ?, ?).

foldl1(Goal, [V0|List], V) :-
  foldl(Goal, List, V0, V).

replace0(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R).

times(A,B,C) :-
  (   var(A)
  ->  A is C/B
  ;   var(B)
  ->  B is C/A
  ;   C is A*B
  ).

mul_list(Xs, Res) :-
  foldl1(times, Xs, Res).
