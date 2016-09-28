:- module(grammar, [grammar/2,statement/2]).
grammar(Ls,O):- grammar(Ls,[],O).

grammar([],O,O).
grammar([H|T],A,O):-statement(H,H1),grammar(T,[H1|A],O).

statement([(ord,add),D,S1,S2],[(ord,add),D1,S11,S21]):-destinationTmp(D,D1),sourceTmp(S1,S11),sourceTmp(S2,S21).


sourceTmp((temp,X),(sourcetemp,X)).
destinationTmp((temp,X),(desttemp,X)).
