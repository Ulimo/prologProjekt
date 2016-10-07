:- module(ranges, [buildRanges/2]).


buildRanges(List, Output) :-
    createUniqueList(List, Variables),
    buildRange(List, Variables, [], OutputBuild),
    printFile(OutputBuild, "ranges.txt"),
    Output = OutputBuild.

buildRange(List, [], A, A).
buildRange(List, [H|T], A, Output) :-
    find(List, H, FindOutput),
    createTuples(FindOutput, [], Tuples),
    buildRange(List, T, [(H,Tuples)|A], Output).

find(List, Var, Output) :-
    findStart(List, Var, 0, [], Output).
    %! findRangeNew(List, Var, 0, 0, [], Output).


createTuples([], A, Output) :-
    reverse(A, Output).
createTuples([H1, H2|T], A, Output) :-
    createTuples(T, [(H1, H2)|A], Output).
    
findStart([], Var, Index, A, Output) :-
    reverse(A, Output).
findStart([H|T], Var, Index, A, Output) :-
    memberchk((_, Var), H),
    K is Index + 1,
    findRange(T, Var, K, [Index|A], Output).
findStart([H|T], Var, Index, A, Output) :- %! No variable here, continue to iterate
    K is Index + 1,
    findStart(T, Var, K, A, Output).
    
    
findRange([], Var, Index, A, Output) :-
    Old is Index - 1,
    reverse([Old|A], Output).
findRange([H|T], Var, Index, A, Output) :-
    memberchk((s, Var), H),
    K is Index + 1,
    findRange(T, Var, K, A, Output).
findRange([H|T], Var, Index, A, Output) :-
    memberchk((d, Var), H),
    Old is Index - 1,
    K is Index + 1,
    findRange(T, Var, K, [Index,Old|A], Output).
findRange([H|T], Var, Index, A, Output) :-
    Old is Index - 1,
    K is Index + 1,
    findStart(T, Var, K, [Old|A], Output).


findRangeNew([], Var, Index, 0, A, Output) :-
    reverse(A, Output).
findRangeNew([], Var, Index, 1, A, Output) :-
    Old is Index - 1,
    reverse([Old|A], Output).
findRangeNew([H|T], Var, Index, 0, A, Output) :-
    memberchk((_, Var), H),
    K is Index + 1,
    findRangeNew(T, Var, K, 1, [Index|A], Output).
findRangeNew([H|T], Var, Index, 1, A, Output) :-
    memberchk((s, Var), H),
    K is Index + 1,
    findRangeNew(T, Var, K, 1, A, Output).
findRangeNew([H|T], Var, Index, 1, A, Output) :-
    memberchk((d, Var), H),
    Old is Index - 1,
    K is Index + 1,
    findRangeNew(T, Var, K, 1, [Index,Old|A], Output).
findRangeNew([H|T], Var, Index, 1, A, Output) :-
    Old is Index - 1,
    K is Index + 1,
    findRangeNew(T, Var, K, 0, [Old|A], Output).
findRangeNew([H|T], Var, Index, 0, A, Output) :- %! No variable here, continue to iterate
    K is Index + 1,
    findRangeNew(T, Var, K, 0, A, Output).
    
    
printFile(List, File) :-
    open(File,write,OS), %! open the file
    printFileInternal(List, OS).

printFileInternal([], OS) :-
    close(OS).
printFileInternal([(V,H)|T], OS) :-
    write(OS, V),
    write(OS, ": "),
    printFileLine(H, OS),
    write(OS, "\n"), %! add a new line
    printFileInternal(T, OS).

printFileLine([], OS).
printFileLine([(R1,R2)|T], OS) :-
    write(OS, "("),
    write(OS, R1),
    write(OS, ", "),
    write(OS, R2),
    write(OS, ") "),
    printFileLine(T, OS).
    
    
createUniqueList(List, Output) :-
    createUniqueList(List, [], Output).
    
createUniqueList([], A, A).
createUniqueList([H|T], A, Output) :-
    createUniqueListInner(H, A, OutputInner),
    createUniqueList(T, OutputInner, Output).
    
createUniqueListInner([], A, A).
createUniqueListInner([(_,H)|T], A, Output) :-
    addToList(H, A, L),
    createUniqueListInner(T, L, Output).
    
addToList(Element, List, Output) :-
    memberchk(Element, List),
    Output = List.
addToList(Element, List, Output) :-
    Output = [Element|List].