:- module(graph, [createGraph/2, removeUnderDegree/3, filterVertices/4]).

createGraph(Vertices, Output) :-
    open("edges.txt", write, OS),
    close(OS),
    createGraphInternal(Vertices, Output).
    
createGraphInternal(Vertices, Output) :-
    createEdges(Vertices, [], Edges),
    createVerticeList(Vertices, Edges, [], VertexList),
    printFile((VertexList, Edges), "edges.txt"),
    Output = (VertexList, Edges).
    

createVerticeList([], _, A, A).
createVerticeList([VH|VT], Edges, A, Output) :-
    countDegree(VH, Edges, 0, Count),
    createVerticeList(VT, Edges, [(VH, Count)|A], Output).
    
countDegree(_, [], Count, Count).
countDegree(Vertex, [(Vertex,_)|ET], Count, Output) :-
    K is Count + 1,
    countDegree(Vertex, ET, K, Output).
countDegree(Vertex, [(_,Vertex)|ET], Count, Output) :-
    K is Count + 1,
    countDegree(Vertex, ET, K, Output).
countDegree(Vertex, [_|ET], Count, Output) :-
    countDegree(Vertex, ET, Count, Output).

createEdges([], A, A).
createEdges([H|T], A, Output) :-
    checkBounds(H, T, A, Acc),
    createEdges(T, Acc, Output).
    

checkBounds(_, [], A, A).
checkBounds(Var, [Cmp|T], A, Output) :-
    overlap(Var, Cmp),
    checkBounds(Var, T, [(Var, Cmp)|A], Output).
checkBounds(Var, [_|T], A, Output) :-
    checkBounds(Var, T, A, Output).



overlap((_, Start, End),(_, CmpStart, _)) :-
    CmpStart >= Start, CmpStart =< End.
overlap((_, Start, End),(_, _, CmpEnd)) :-
    CmpEnd >= Start, CmpEnd =< End.
overlap((_, Start, End),(_, CmpStart, CmpEnd)) :-
    CmpStart =< Start, CmpEnd >= End.
    

removeUnderDegree(G, Degree, Output) :-
    removeUnderDegree(G, Degree, [], Output). 

removeUnderDegree(Compare, _, Compare, Compare). %! Fixed point
removeUnderDegree((V, E), Degree, _, Output) :-
    filterVertices(V, Degree, [], Cmp),
    removeUnderDegree(Cmp, Degree, (V,E), Output).
    
filterVertices([], _, A, Output) :-
    createGraphInternal(A, Output).
    %! createEdges(A, [], Edges),
    %! createVerticeList(A, Edges, [], VertexList),
    %! Output = (VertexList, Edges).
filterVertices([(V, Deg)|T], Degree, A, Output) :-
    Deg >= Degree,
    filterVertices(T, Degree, [V|A], Output).
filterVertices([_|T], Degree, A, Output) :-
    filterVertices(T, Degree, A, Output).

    
printFile((V,E), File) :-
    open(File,append,OS), %! open the file
    write(OS, "Vertices: \n"),
    printFileVertices(V, OS),
    write(OS, "\nEdges: \n"),
    printFileEdges(E, OS),
    write(OS, "\n\n"),
    close(OS).


    
printFileVertices([], _).
printFileVertices([(V, Degree)|T], OS) :-
    write(OS, "("),
    printFileTuple(V, OS),
    write(OS, ", "),
    write(OS, Degree),
    write(OS, ")"),
    write(OS, "\n"),
    printFileVertices(T, OS).
    
printFileEdges([], _).
printFileEdges([(V1, V2)|T], OS) :-
    write(OS, "("),
    printFileTuple(V1, OS),
    write(OS, ", "),
    printFileTuple(V2, OS),
    write(OS, ") "),
    write(OS, "\n"), %! add a new line
    printFileEdges(T, OS).
    
printFileTuple((Var, Start, End), OS) :-
    write(OS, "("),
    write(OS, Var),
    write(OS, ", "),
    write(OS, Start),
    write(OS, ", "),
    write(OS, End),
    write(OS, ")").