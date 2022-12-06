:- module(lists_ext, [exclude/3]).

:- meta_predicate(exclude(1, ?, ?)).

exclude(Goal, Ls0, Ls) :-
  exclude_(Ls0, Goal, Ls).

exclude_([], _, []).
exclude_([L|Ls0], Goal, Ls) :-
  (   call(Goal, L) ->
      Ls = Rest
  ;   Ls = [L|Rest]
  ),
  exclude_(Ls0, Goal, Rest).

