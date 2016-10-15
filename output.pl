:- module(output, [createOutput/3]).
:- use_module(registers).
:- use_module(grammar).

createOutput(Read, Allocations, File) :-
    open(File,write,OS),
    reverse(Read, ReadReverse),
    buildOutput(ReadReverse, Allocations, 0, OS),
    close(OS).

buildOutput([], _, _, _).
buildOutput([H|T], Allocations, Line, OS) :-
    print(H, Line, Allocations, OS),
    write(OS, "\n"),
    K is Line + 1,
    buildOutput(T, Allocations, K, OS).
    

print([Var|T], LineIndex, Allocations, OS) :- 
    command(Var),
    write(OS, Var),
    write(OS, " "),
    printVariables(T, LineIndex, Allocations, OS).


printVariables([], _, _, _).
printVariables([Var|[]], LineIndex, Allocations, OS) :-
    getRegister(Var, LineIndex, Allocations, Register),
    r(Register, RegName),
    write(OS, RegName).
printVariables([Var|T], LineIndex, Allocations, OS) :-
    getRegister(Var, LineIndex, Allocations, Register),
    r(Register, RegName),
    write(OS, RegName),
    write(OS, ", "),
    printVariables(T, LineIndex, Allocations, OS).
printVariables([Var|[]], _, _, OS) :-
    write(OS, Var).
printVariables([Var|T], LineIndex, Allocations, OS) :-
    write(OS, Var),
    write(OS, ", "),
    printVariables(T, LineIndex, Allocations, OS).


    
getRegister(Var, Index, [(((Var, Start, End), Degree), Register)|T], Register) :-
    Index =< End,
    Index >= Start.
getRegister(Var, Index, [_|T], Output) :-
    getRegister(Var, Index, T, Output).