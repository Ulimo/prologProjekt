:- module(readfile, [readfile/2]).


use_module(library(pio)).



lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).


readfile(File, K) :- phrase_from_file(lines(Ls), File), parseLines(Ls, K).

parseLines(Ls, K) :-
    parseLines(Ls, [], K).
    
parseLines([], O, O).
 parseLines([[]|T], K, O) :- 
    parseLines(T, K, O).
parseLines([H|T], K, O) :-
    splitString(H, [], [], K1),
    addList(T, K1, K, O).
    
addList(T, [], A, O) :- %! Skip blank rows
    parseLines(T, A, O).
addList(T, L, A, O) :-
    parseLines(T, [L|A], O).
    
    
splitString([], [], A2, O) :-
    reverse(A2, O).
splitString([], A1, A2, O) :-
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString([], [], [OrdAtom|A2], O).
splitString([H|T], [], A2, O) :-
    H == 32, %! Blankspace
    splitString(T, [], A2, O).
splitString([H|T], A1, A2, O) :-
    H == 32, %! Blankspace
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString(T, [], [OrdAtom|A2], O).
splitString([H|T], [], A2, O) :-
    H == 44, %! Comma
    splitString(T, [], A2, O).
splitString([H|T], A1, A2, O) :-
    H == 44, %! Comma
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString(T, [], [OrdAtom|A2], O).
splitString([H|T], [], A2, O) :-
    H == 59, %! Semicolon
    readComments([H|T], [], A2, O).
splitString([H|T], A1, A2, O) :- %! Handles comments in assembler
    H == 59, %! semicolon
    reverse(A1, O1),
    readComments([H|T], [], [O1|A2], O).
splitString([H|T], A1, A2, O) :-
    splitString(T, [H|A1], A2, O).
    
readComments([], _, A2, O) :-
    reverse(A2, O).
readComments([H|T], A1, A2, O) :-
    readComments(T, [H|A1], A2, O).