:- module('13', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(aggregate).
:- use_module(lists_ext).

% parse {{{
blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(I) --> digit(D), integer_(Ds), { number_chars(I, [D|Ds]) }.
integer_([D|Ds]) --> digit(D), !, integer_(Ds).
integer_([]) --> [].

item(I) --> integer(I), !.
item(L) --> list(L).

list([]) --> "[", blanks, "]", !.
list([I|Is]) --> "[", blanks, item(I), blanks, list_(Is), blanks, "]".
list_([I|Is]) --> ",", blanks, item(I), !, blanks, list_(Is).
list_([]) --> [].

pair([L1, L2]) --> list(L1), nl, list(L2).

parse([P|Ps]) --> blanks, pair(P), parse_(Ps), blanks.
parse_([P|Ps]) --> nl, nl, pair(P), !, parse_(Ps).
parse_([]) --> [].
% }}}

% cmp {{{
cmp(I1, I2, Res) :-
  integer(I1), integer(I2), !,
  Res is I1 - I2.
cmp([], [], 0) :- !.
cmp([], [_|_], -1) :- !.
cmp([_|_], [], 1) :- !.
cmp(L1, I2, Res) :-
  is_list(L1), integer(I2), !,
  cmp(L1, [I2], Res).
cmp(I1, L2, Res) :-
  integer(I1), is_list(L2), !,
  cmp([I1], L2, Res).
cmp([E1|L1], [E2|L2], Res) :-
  cmp(E1, E2, Res0),
  (   Res0 \== 0
  ->  Res is Res0
  ;   cmp(L1, L2, Res)
  ).
% }}}

% ordered list {{{
split_half(List, LeftLen, Left, Pivot, Right) :-
  length(List, Len),
  Len > 0,
  LeftLen is Len div 2,
  RightLen is max(0, Len - LeftLen - 1),
  length(Left, LeftLen),
  length(Right, RightLen),
  append([Left, [Pivot], Right], List).

bs_sum_res(I0, found(I1), found(I)) :- I is I0 + I1 + 1.
bs_sum_res(I0, notfound(I1), notfound(I)) :- I is I0 + I1 + 1.

bs_index_res(found(I), I).
bs_index_res(notfound(I), I).

binary_search0([], _, notfound(0)) :- !.
binary_search0(List, N, Res) :-
  split_half(List, I, Left, Pivot, Right),
  cmp(Pivot, N, Cmp),
  (   Cmp == 0
  ->  Res = found(I)
  ;   Cmp > 0
  ->  binary_search0(Left, N, Res)
  ;   binary_search0(Right, N, Res0),
      bs_sum_res(I, Res0, Res)
  ).

binary_search1(List, N, Res) :-
  binary_search0(List, N, Res0),
  bs_sum_res(0, Res0, Res).

ol_add(List0, E, List) :-
  binary_search0(List0, E, I0),
  bs_index_res(I0, I),
  nth0(I, List, E, List0).

list_to_ol(List, Res) :-
  list_to_ol_(List, [], Res).
list_to_ol_([], Res, Res) :- !.
list_to_ol_([E|List], Acc0, Res) :-
  ol_add(Acc0, E, Acc),
  list_to_ol_(List, Acc, Res).

ol_index1(List, E, Res) :- binary_search1(List, E, found(Res)).
% }}}

part1(Data, Res) :-
  length(Data, Len),
  sumall(I, (
    between(1, Len, I),
    nth1(I, Data, Pair),
    Call =.. [cmp|Pair],
    call(Call, R),
    R < 1
  ), Res).
part2(Data, Res) :-
  foldl1(append, Data, Flat0),
  Dividers = [[[2]], [[6]]],
  append(Flat0, Dividers, Flat),
  list_to_ol(Flat, List),
  maplist(ol_index1(List), Dividers, Indexes),
  mul_list(Indexes, Res).
