:- module(grammar, [grammar/2, command/1]).
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

rcmd(add)-->[add].
rcmd(sub)-->[sub].
rcmd(mul)-->[mul].
rcmd(div)-->[div].

icmd(addi)-->[addi].
icmd(subi)-->[subi].
icmd(muli)-->[muli].
icmd(divi)-->[divi].

lwsw(sw) --> [sw].
lwsw(ld)-->[ld].

mem(X) --> [X].

addToList(X, O, A) --> reg(Z), {O = A, !}.
addToList(X, O, A) --> [X], {O = [X|A]}.

reg(X)-->[X], {r(_, X)}.

imm(X)-->[X],{atom_number(X,_)}.


command(X) :-
    command(_, [X], []).
command(X) --> rcmd(X);icmd(X);lwsw(X).