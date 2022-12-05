:- module('1', [parse//1, part1/2, part2/2]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

blank --> [C], { char_type(C, whitespace) }.
blanks --> blank, !, blanks.
blanks --> [].

nl --> "\n".

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(I) --> digit(D), integer_(Ds), { number_chars(I, [D|Ds]) }.
integer_([D|Ds]) --> digit(D), !, integer_(Ds).
integer_([]) --> [].

elf(E) --> integer(I), elf_(Is), { sum_list([I|Is], E) }.
elf_([I|Is]) --> nl, integer(I), !, elf_(Is).
elf_([]) --> [].

parse(Es) -->
  blanks, elf(E), !, parse_(Es0), blanks,
  { list_to_ord_set([E|Es0], Es) }.
parse_([E|Es]) --> nl, nl, elf(E), !, parse_(Es).
parse_([]) --> [].

part1(Data, Res) :- once(append(_, [Res], Data)).
part2(Data, Res) :-
  length(Top3, 3),
  once(append(_, Top3, Data)),
  sum_list(Top3, Res).
