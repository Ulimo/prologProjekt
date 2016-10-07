:- module(liveness, [buildLiveness/2]).

%! gets a reverse list
buildLiveness(List, Output) :-
    reverse(List, ListReverse),
    buildLiveness(ListReverse, [[]], [H|T]),
    %! forwardPass(List, T, [], [], ForwardOutput),
    printFile(T, "liveness.txt"),
    Output = T.

buildLiveness([], A, A).
buildLiveness([[(cmd, _), (desttemp, Destination), (sourcetemp, Source1), (sourcetemp, Source2)]|T], [AH|AT], O) :-
    %! removeDestination(Destination, AH, A1),
    addDestination(Destination, AH, Ad),
    addSource(Source1, Ad, A2),
    addSource(Source2, A2, A3),
    removeDestination(Destination, A3, A4),
    %! addSource(Source1, A4, A5),
    %! addSource(Source2, A5, Next),
    buildLiveness(T, [A4,A3|AT], O).
buildLiveness([[(cmdi, _), (desttemp, Destination), (sourcetemp, Source1), (imm, _)]|T], [AH|AT], O) :-
    %! removeDestination(Destination, AH, A1),
    addDestination(Destination, AH, A1),
    addSource(Source1, A1, A2),
    removeDestination(Destination, A2, A3),
    %! removeDestination(Destination, A2, A3),
    %! addSource(Source1, A3, Next),
    buildLiveness(T, [A3, A2|AT], O).

    %! checkDestination(Destination, A),
    %! checkSource(Source1, A),
    %! checkSource(Source2, A).
    
removeDestination(Destination, List, Output) :-
    delete(List, (d, Destination), Output).
removeDestination(Destination, List, List).

removeSource(Source, List, Output) :-
    delete(List, (s, Source), Output).
removeSource(Source, List, List).


forwardPass([], [[]], _, A, Output) :-
    reverse(A, Output).
forwardPass([], [], _, A, Output) :-
    reverse(A, Output).
forwardPass([[(cmdi, _), (desttemp, Destination), (sourcetemp, Source1), (imm, _)]|T], [H|Tail], Defined, A, Output) :-
    addToList(Source1, Defined, Defined1),
    addToList(Destination, Defined1, Defined2),
    removeVariables(H, Defined2, [], LineOutput),
    forwardPass(T,Tail, Defined2, [LineOutput|A], Output).
forwardPass([[(cmd, _), (desttemp, Destination), (sourcetemp, Source1), (sourcetemp, Source2)]|T], [H|Tail], Defined, A, Output) :-
    addToList(Source1, Defined, Defined1),
    addToList(Source2, Defined1, Defined2),
    addToList(Destination, Defined2, Defined3),
    removeVariables(H, Defined3, [], LineOutput),
    forwardPass(T,Tail, Defined3, [LineOutput|A], Output).

removeVariables([], _, A, A).
removeVariables([(B,H)|T], Defined, A, Output) :-
    memberchk(H, Defined),
    removeVariables(T, Defined, [(B,H)|A], Output).
removeVariables([(B,H)|T], Defined, A, Output) :-
    removeVariables(T, Defined, A, Output).

printFile(List, File) :-
    open(File,write,OS), %! open the file
    printFileInternal(List, OS).

printFileInternal([], OS) :-
    close(OS).
printFileInternal([H|T], OS) :-
    printFileLine(H, OS),
    write(OS, "\n"), %! add a new line
    printFileInternal(T, OS).

printFileLine([], OS).
printFileLine([(d,H)|T], OS) :-
    write(OS, "+"),
    write(OS, H),
    write(OS, ", "),
    printFileLine(T, OS).
printFileLine([(s,H)|T], OS) :-
    write(OS, H),
    write(OS, ", "),
    printFileLine(T, OS).
    
addDestination(Destination, List, Output) :-
    removeSource(Destination, List, O1),
    Output = [(d,Destination)|O1].
    
addToList(Element, List, Output) :-
    memberchk(Element, List),
    Output = List.
addToList(Element, List, Output) :-
    Output = [Element|List].


addSource(Source, List, Output) :-
    removeDestination(Source, List, O1),
    memberchk((s,Source), O1),
    Output = O1.
addSource(Source, List, Output) :-
    removeDestination(Source, List, O1),
    Output = [(s,Source)|O1]. %! Add tuple with s meaning source, variable has not changed.
    
%! calculateRanges(LivenessList, ActiveList, )