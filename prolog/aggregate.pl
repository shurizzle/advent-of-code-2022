:- module(aggregate, [foldall/5, countall/3, sumall/3, minall/3]).

:- meta_predicate foldall(3, ?, 0, ?, ?).
:- meta_predicate countall(?, 0, ?).
:- meta_predicate sumall(?, 0, ?).
:- meta_predicate minall(?, 0, ?).

:- use_module(library(iso_ext)).
:- use_module(library(dif)).

foldall_nesting(Value, C, ID) :-
  (   bb_get(i_foldall_counter, C0) ->
      C is C0 + 1
  ;   C = 0
  ),
  number_chars(C, Cs),
  atom_chars(Atom, Cs),
  atom_concat(i_foldall_nesting_, Atom, ID),
  bb_put(ID, +Value),
  bb_put(i_foldall_counter, C).

foldall(Reducer, Template, Goal, V0, V) :-
  foldall_nesting(V0, C, ID),
  (   setup_call_cleanup(true,
                         (   Goal,
                             bb_get(ID, +V1),
                             (    call(Reducer, Template, V1, V2)
                             ->   bb_put(ID, +V2)
                             ;    bb_put(ID, false), !
                             ),
                             fail
                         ),
                         (   bb_get(i_foldall_counter, C) ->
                             C1 is C - 1,
                             bb_put(i_foldall_counter, C1)
                         ;   true
                         ))
  ;   setup_call_cleanup(true,
                         bb_get(ID, +V),
                         bb_put(ID, nil))
  ).

count_(_, Acc0, Acc) :- Acc is Acc0+1.

sum_(V, Acc0, Acc) :- Acc is Acc0+V.

min_(V, Acc0, Acc) :-
  (   Acc0 == nil
  ->  Acc = V
  ;   Acc is min(Acc0, V)
  ).

countall(Template, Goal, V) :-
  foldall(aggregate:count_, Template, Goal, 0, V).

sumall(Template, Goal, V) :-
  foldall(aggregate:sum_, Template, Goal, 0, V).

minall(Template, Goal, V) :-
  foldall(aggregate:min_, Template, Goal, nil, V0),
  dif(V0, nil),
  V = V0.
