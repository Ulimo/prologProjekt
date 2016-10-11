:- module(grammar2, [grammar/2,statement/2]).
grammar(Ls,O):- grammar(Ls,[],O).

grammar([],O,O).
grammar([H|T],A,O):-statement(H,H1),grammar(T,[H1|A],O).

%!statement([Cmd,Destination,Source1,Source2],(Cmd,[Destination],[Source1,Source2]))-->cmd.
%!statement([Cmd,Destination,Source1,Source2],(Cmd,[Destination],[Source1,Source2]))-->reg_command(Cmd).

%!statement(A)-->reg_command(B),destination(C),source(D),source(E),{A = (B,[C],[D,E])}.
%!statement(A)-->imm_command(B),destination(C),source(D),imm,{A = (B,[C],[D])}.

%!reg_command(add)-->[add].
%!reg_command(sub)-->[sub].
%!imm_command(addi)-->[addi].
%!imm_command(subi)-->[subi].
%!destination(X)-->[X].
%!source(X)-->[X].

statement(A)-->reg_command(A).
statement(A)-->imm_command(A).
reg_command(A)-->rcmd(B),addToList(C,D,[]),addToList(E,S1,[]),addToList(F,S,S1),{A = (B,S,D)}.
imm_command(A)-->icmd(B),addToList(C,D,[]),addToList(E,S,[]),imm(I),{A = (B,S,D)}.

rcmd(add)-->[add].
rcmd(sub)-->[sub].

icmd(addi)-->[addi].
icmd(subi)-->[subi].

addToList(X, O, A) --> reg, {O = A, !}.
addToList(X, O, A) --> [X], {O = [X|A]}.

destination(X)-->member(X,[R0,'zero']).
destination(X)-->[X].

reg-->[r1].
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