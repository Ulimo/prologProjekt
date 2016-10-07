:- module(graph, [overlap/4, createGraph/2]).

createGraph(Vertices, Output) :-
    createEdges(Vertices, [], Output).

createEdges([], A, A).
createEdges([H|T], A, Output) :-
    checkBounds(H, T, A, Acc),
    createEdges(T, Acc, Output).
    

checkBounds(_, [], A, A).
checkBounds(Var, [Cmp|T], A, Output) :-
    overlap(Var, Cmp),
    checkBounds(Var, T, [(Var, Cmp)|A], Output).
checkBounds(Var, [B|T], A, Output) :-
    checkBounds(Var, T, A, Output).



overlap((Var, Start, End),(CmpVar, CmpStart, CmpEnd)) :-
    CmpStart > Start, CmpStart < End.
overlap((Var, Start, End),(CmpVar, CmpStart, CmpEnd)) :-
    CmpEnd > Start, CmpEnd < End.
overlap((Var, Start, End),(CmpVar, CmpStart, CmpEnd)) :-
    CmpStart < Start, CmpEnd > End.