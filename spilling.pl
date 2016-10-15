:- module(spilling, [spill/5]).


spill(Liveness, (V,_), ReadData,RegCount, O) :-
    reverse(ReadData, ReadReverse),
    calculateUses(Liveness, V, [], O1),
    findMinimumCost2(O1, Lowest),
    write(Lowest),
    insertStore(ReadReverse, Liveness, Lowest, 0, RegCount, [], O).
    

 insertStore([], [], _, _,  _, _, _) :- fail.
insertStore([H|T], [LH|LT], ((Vertex, Start,End),Degree, Count), Index, RegCount, A, Output) :-
    Index =< Start,
    K is Index + 1,
    insertStore(T, LT, ((Vertex, Start,End),Degree, Count), K,RegCount, [H|A], Output).
insertStore([H|T], [LH|LT], ((Vertex, Start,End),Degree, Count), Index, RegCount, A, Output) :-
    memberchk(Vertex, H),
    insertStore(T, LT, ((Vertex, Start,End),Degree, Count), Index,RegCount, [H|A], Output).
insertStore([H|T], [LH|LT], ((Vertex, Start,End),Degree, Count), Index, RegCount, A, Output) :-
    length(LH, LHLength),
    LHLength =< RegCount,!,
    insertStore(T, LT, ((Vertex, Start,End),Degree, Count), Index,RegCount, [H|A], Output).
insertStore(List, Liveness, ((Vertex, Start,End),Degree, Count), Index, RegCount, A, Output) :-
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



findMinimumCost2(List, Output) :-
    findMinimumCost(List, 99999.0, [], Output).

findMinimumCost([], _, Vertex, Vertex).
findMinimumCost([((Vertex, Start,End), Degree, Uses)|T], Minimum, _, Output) :-
    Cost is Uses / ((Degree + End - Start)),
    Cost < Minimum,
    findMinimumCost(T, Cost, ((Vertex, Start,End), Degree, Uses), Output).
findMinimumCost([(Vertex, Degree, Uses)|T], Minimum, V, Output) :-
    findMinimumCost(T, Minimum, V, Output).
    

%! UTILITY FUNCTIONS

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