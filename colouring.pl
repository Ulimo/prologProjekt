:- module(colouring, [generate/5,colour/3, degreeToList/3]).

colour((Vs,Es),Degree,Output):-
    degreeToList(Degree,[],RegisterList),
    generate(Vs,Es,RegisterList,[],Output),
    testColouring(Es,Output).

degreeToList(0,A,A) :- !.
degreeToList(Degree,A,RegisterList):-
    Next is Degree-1,
    degreeToList(Next,[Degree|A],RegisterList).

generate([],_,_,A,A).
generate([HVs|TVs],Es, Colors,A, Output):-
    member(C,Colors),
    generate(TVs,Es,Colors,[(HVs,C)|A],Output).

testColouring([],_).
testColouring([((V1),(V2))|Te],ColVs):-
    member(((V1,_),C1),ColVs),
    member(((V2,_),C2),ColVs),
    C1\==C2,
    testColouring(Te,ColVs).