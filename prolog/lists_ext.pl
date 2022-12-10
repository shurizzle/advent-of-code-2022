:- module(lists_ext, [exclude/3, scanl/4]).

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
