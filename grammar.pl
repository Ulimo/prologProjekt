:- module(grammar, [grammar/2,statement/2]).
grammar(Ls,O):- grammar(Ls,[],O).

grammar([],O,O).
grammar([H|T],A,O):-statement(H,H1),grammar(T,[H1|A],O).

statement([(ord,C),D,S1,S2],[(cmd,C),D1,S11,S21]):-reg_command(C),destinationTmp(D,D1),sourceTmp(S1,S11),sourceTmp(S2,S21).
statement([(ord,C),D,S,I],[(cmdi,C),D1,S1,I1]):-imm_command(C),destinationTmp(D,D1),sourceTmp(S,S1),immidiate(I,I1).
statement(Else,Else).


reg_command(add).
reg_command(sub).
imm_command(subi).
imm_command(addi).

immidiate((ord,X),(imm,X)).
sourceTmp((temp,X),(sourcetemp,X)).
destinationTmp((temp,X),(desttemp,X)).
