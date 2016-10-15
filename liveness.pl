:- module(liveness, [buildLiveness/2]).

%! gets a reverse list
buildLiveness(List, Output) :-
    reverse(List, ListReverse),
    buildLiveness(ListReverse, [[]], [_|T]),
    printFile(T, "liveness.txt"),
    Output = T.
    
buildLiveness([], A, A).
buildLiveness([(_, Source, Destination)|T], [AH|AT], O) :-
    addDestinations(Destination, AH, A1),
    addSources(Source, A1, A2),
    removeDestinations(Destination, A2, A3),
    clearUsed(A3, [], A4),
    buildLiveness(T, [A4,A2|AT], O).
    
clearUsed([], A, A).
clearUsed([(S, Var, _)|T], A, O) :-
    clearUsed(T, [(S,Var, n)|A], O).
    
addDestinations([], A, A).
addDestinations([H|T], A, O) :-
    addDestination(H, A, A1),
    addDestinations(T, A1, O).
    
addSources([], A, A).
addSources([H|T], A, O) :-
    addSource(H, A, A1),
    addSources(T, A1, O).
    
removeDestinations([], A, A).
removeDestinations([H|T], A, O) :-
    removeDestination(H, A, A1),
    removeDestinations(T, A1, O).
    
removeDestination(Destination, List, Output) :-
    delete(List, (d, Destination, _), Output).
removeDestination(_, List, List).

removeSource(Source, List, Output) :-
    delete(List, (s, Source, _), Output).
removeSource(_, List, List).
    
addDestination(Destination, List, Output) :-
    removeSource(Destination, List, O1),
    Output = [(d,Destination, u)|O1].

addSource(Source, List, Output) :-
    removeDestination(Source, List, O1),
    removeSource(Source, O1, O2),
    Output = [(s,Source, u)|O2]. %! Add tuple with s meaning source, variable has not changed.

%! END OF LIVENESS CODE
    
%! PRINT TO FILE

printFile(List, File) :-
    open(File,write,OS), %! open the file
    printFileInternal(List, OS).

printFileInternal([], OS) :-
    close(OS).
printFileInternal([H|T], OS) :-
    printFileLine(H, OS),
    write(OS, "\n"), %! add a new line
    printFileInternal(T, OS).

printFileLine([], _).
printFileLine([(d,H,u)|T], OS) :-
    write(OS, "+"),
    write(OS, "!"),
    write(OS, H),
    write(OS, ", "),
    printFileLine(T, OS).
printFileLine([(s,H,u)|T], OS) :-
    write(OS, "!"),
    write(OS, H),
    write(OS, ", "),
    printFileLine(T, OS).
printFileLine([(d,H,n)|T], OS) :-
    write(OS, "+"),
    write(OS, H),
    write(OS, ", "),
    printFileLine(T, OS).
printFileLine([(s,H,n)|T], OS) :-
    write(OS, H),
    write(OS, ", "),
    printFileLine(T, OS).
    
