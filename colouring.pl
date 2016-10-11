:- module(colouring, [generate/5,colour/3, degreeToList/3]).

colour((Vs,Es),Degree,Output):-
    degreeToList(Degree,[],RegisterList),
    generate(Vs,Es,RegisterList,[],Output),
    writeFile(Output, "color.txt").
    %! testColouring(Es,Output).


generate([],_,_,A,A).
generate([HVs|TVs],Es, Colors,A, Output):-
    member(C,Colors),
    testNode((HVs,C),Es,A),
    generate(TVs,Es,Colors,[(HVs,C)|A],Output).

degreeToList(0,A,A) :- !.
degreeToList(Degree,A,RegisterList):-
    Next is Degree-1,
    degreeToList(Next,[Degree|A],RegisterList).

testColouring([],_).

testColouring([((V1),(V2))|Te],ColVs):-
    member(((V1,_),C1),ColVs),
    member(((V2,_),C2),ColVs),
    C1\==C2,
    testColouring(Te,ColVs).

testNode(_,[],_).

testNode(((V,D),C),[(V,CmpV)|TEs],ColVs):-
    member(((CmpV,_),C),ColVs),!,fail.
    %!testNode(((V,D),C),TEs,ColVs).
testNode(((V,D),C),[(CmpV,V)|TEs],ColVs):-
    testNode(((V,D),C),[(V,CmpV)|TEs],ColVs).

testNode(V,[_|TEs],ColVs):-
    testNode(V,TEs,ColVs).

writeFile(Input, File) :-
    open(File,write,OS),
    writeToFile(Input, OS).
    
writeToFile([], OS) :-
    close(OS).
writeToFile([Var|T], OS) :-
    write(OS, Var),
    write(OS, "\n"),
    writeToFile(T, OS).