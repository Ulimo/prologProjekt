:- module(spilling, [spill/4]).


spill(Liveness, (V,_), ReadData, O) :-
    reverse(ReadData, ReadReverse),
    calculateUses(Liveness, V, [], O1),
    findMinimumCost2(O1, Lowest), %! Cut här
    write(Lowest),
    insertStore(ReadReverse, Liveness, Lowest, 0, [], O).
    

 insertStore([], [], _, _, _, _) :- fail.
insertStore([H|T], [LH|LT], ((Vertex, Start,End),Degree, Count), Index, A, Output) :-
    Index =< Start,
    K is Index + 1,
    insertStore(T, LT, ((Vertex, Start,End),Degree, Count), K, [H|A], Output).
insertStore([H|T], [LH|LT], ((Vertex, Start,End),Degree, Count), Index, A, Output) :-
    memberchk(Vertex, H),
    insertStore(T, LT, ((Vertex, Start,End),Degree, Count), Index, [H|A], Output).
insertStore([H|T], [LH|LT], ((Vertex, Start,End),Degree, Count), Index, A, Output) :-
    length(LH, LHLength),
    LHLength =< 3,
    insertStore(T, LT, ((Vertex, Start,End),Degree, Count), Index, [H|A], Output).
insertStore(List, Liveness, ((Vertex, Start,End),Degree, Count), Index, A, Output) :-
    atom_concat(mem, Vertex, MemName),
    insertLoad(List, ((Vertex, Start,End),Degree, Count), Index, [['sw', Vertex, MemName]|A], Output).
    
    
insertLoad([], _, _, A, A).
insertLoad([H|T], ((Vertex, Start,End),Degree, Count), Index, A, Output) :-
    Index =< End,
    memberchk(Vertex, H),
    atom_concat(mem, Vertex, MemName),
    doRest(T, [H, ['ld', Vertex, MemName]|A], Output).
insertLoad([H|T], ((Vertex, Start,End),Degree, Count), Index, A, Output) :-
    K is Index + 1,
    insertLoad(T, ((Vertex, Start,End),Degree, Count), K, [H|A], Output).

    %! insertStore(T, ((Vertex, Start,End),Degree, Count), K, [H|A], Output).

doRest([], A, A).
doRest([H|T], A, Output) :-
    doRest(T, [H|A], Output).

calculateUses(Liveness, [], A, A).
calculateUses(Liveness, [(Var,Degree)|T], A, Output) :-
    calculate((Var,Degree), Liveness, O1),
    calculateUses(Liveness, T, [(Var,Degree, O1)|A], Output).
    
calculate(Vertex, Liveness, Output) :-
    goToLine(Vertex, Liveness, 0, 0, Output).
    
goToLine(((Vertex, Start,End),Degree), [H|T], Index, Count, Output) :-
    Index < Start,
    K is Index + 1,
    goToLine(((Vertex, Start,End),Degree), T, K, Count, Output).
goToLine(((Vertex, Start,End),Degree), Liveness, Index, Count, Output) :-
    calculateUsesVertex(((Vertex, Start,End),Degree), Liveness, Index, Count, Output).
    
%! 
calculateUsesVertex(((Vertex, Start,End),Degree), [H|T], Index, Count, Output) :-
    Index =< End,
    memberchk((_,Vertex,u), H),
    K is Count + 1,
    I is Index + 1,
    calculateUsesVertex(((Vertex, Start,End),Degree), T, I, K, Output).
calculateUsesVertex(((Vertex, Start,End),Degree), [H|T], Index, Count, Output) :-
    Index =< End,
    I is Index + 1,
    calculateUsesVertex(((Vertex, Start,End),Degree), T, I, Count, Output).
calculateUsesVertex(_, _, _, Count, Count).

findMinimumCost2(List, Output) :-
    findMinimumCost(List, 99999.0, [], Output).

findMinimumCost([], _, Vertex, Vertex).
findMinimumCost([((Vertex, Start,End), Degree, Uses)|T], Minimum, _, Output) :-
    Cost is Uses / ((Degree + End - Start)),
    Cost < Minimum,
    findMinimumCost(T, Cost, ((Vertex, Start,End), Degree, Uses), Output).
findMinimumCost([(Vertex, Degree, Uses)|T], Minimum, V, Output) :-
    findMinimumCost(T, Minimum, V, Output).