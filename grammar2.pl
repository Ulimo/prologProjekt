:- module(grammar2, [grammar/2,statement/2, command/1]).
:- use_module(registers).
grammar(Ls,O):- grammar(Ls,[],O).

grammar([],O,O).
grammar([H|T],A,O):-statement(O1,H,[]),grammar(T,[O1|A],O).

statement(A)-->reg_command(A).
statement(A)-->imm_command(A).
statement(A) --> lw_command(A).
statement(A) --> sw_command(A).

reg_command(A)-->rcmd(B),addToList(C,D,[]),addToList(E,S1,[]),addToList(F,S,S1),{A = (B,S,D)}.
imm_command(A)-->icmd(B),addToList(C,D,[]),addToList(E,S,[]),imm(I),{A = (B,S,D)}.
lw_command(A) --> [ld], addToList(C, D, []), mem(Z), {A = (ld, [], D)}.
sw_command(A) --> [sw], addToList(C, S, []), mem(Z), {A = (sw, S, [])}.

command(X) :-
    command(_, [X], []).

command(X) --> rcmd(X);icmd(X);lwsw(X).

rcmd(add)-->[add].
rcmd(sub)-->[sub].

icmd(addi)-->[addi].
icmd(subi)-->[subi].

lwsw(sw) --> [sw].
lwsw(ld)-->[ld].

mem(X) --> [X].

addToList(X, O, A) --> reg(Z), {O = A, !}.
addToList(X, O, A) --> [X], {O = [X|A]}.

destination(X)-->member(X,[R0,'zero']).
destination(X)-->[X].

reg(X)-->[X], {r(_, X)}.

imm(X)-->[X],{atom_number(X,_)}.

source(X)-->[X].
immidiate((ord,X),(imm,X)).
sourceTmp((temp,X),(sourcetemp,X)).
destinationTmp((temp,X),(desttemp,X)).

sentence --> noun_phrase, verb_phrase.
noun_phrase --> [the], noun.
verb_phrase --> [runs].
noun --> [rabbit].
noun --> [engine].


expr(X) --> term(Y), [+], expr(Z), { X is Y+Z}.
expr(X) --> term(Y), [-], expr(Z), { X is Y-Z}.
expr(X) --> term(X).
term(X) --> factor(Y), [*], term(Z), { X is Y*Z}.
term(X) --> factor(Y), [/], term(Z), { X is Y/Z}.
term(X) --> factor(X).
factor(X) --> [X], {integer(X)}.