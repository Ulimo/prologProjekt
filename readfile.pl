:- module(readfile, [readfile/2, tsplit/1, writeToFile/1]).

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
    %! split_string(H, " ,", "", V1),
    %! getAtomics(V1, K1),
    splitString(H, [], [], K1),
    addList(T, K1, K, O).
    %! parseLines(T, [K1|K], O).
    
addList(T, [], A, O) :-
    parseLines(T, A, O).
addList(T, L, A, O) :-
    parseLines(T, [L|A], O).

splitString([], [], A2, O) :-
    reverse(A2, O).
splitString([], A1, A2, O) :-
    checkSection([], A1, A2, O).
     %! reverse(A1, O1),
     %! atom_codes(OrdAtom, O1),
     %! A3 = [(ord,OrdAtom)|A2],
     %! reverse(A3, O).
splitString([H|T], A1, A2, O) :-
    H == 34, %! Qoute sign
    readQoutes(T, [H|A1], A2, O). %! Start reading quotes


splitString([H|T], [], A2, O) :-
    H == 32, %! Blankspace
    splitString(T, [], A2, O).
splitString([H|T], A1, A2, O) :-
    H == 32, %! Blankspace
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString(T, [], [(ord, OrdAtom)|A2], O).


splitString([H|T], [], A2, O) :-
    H == 46, %! Dot, punkt .
    splitString(T, [], A2, O).
splitString([H|T], A1, A2, O) :-
    H == 46, %! Dot, punkt .
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString(T, [], [(ord, OrdAtom)|A2], O).

splitString([H|T], [], A2, O) :-
    H == 44, %! Comma
    splitString(T, [], A2, O).
splitString([H|T], A1, A2, O) :-
    H == 44, %! Comma
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString(T, [], [(ord, OrdAtom)|A2], O).

splitString([H|T], [], A2, O) :-
    H == 59, %! Semicolon
    readComments([H|T], [], A2, O).
splitString([H|T], A1, A2, O) :- %! Handles comments in assembler
    H == 59, %! semicolon
    reverse(A1, O1),
    readComments([H|T], [], [(ord,O1)|A2], O).
splitString([H|T], [], A2, O) :-
    H == 37, 
    splitStringTemporary(T, [], OT, R), %! get one word
    splitString(R, [], [OT|A2], O). %! continue to split the split on the rest of the words
splitString([H|T], A1, A2, O) :-
    H == 37, 
    splitStringTemporary(T, [], OT, R), %! get one word
    reverse(A1, A1Reverse),
    atom_codes(OrdAtom, A1Reverse),
    splitString(R, [], [OT, (ord,OrdAtom)|A2], O). %! continue to split the split on the rest of the words
splitString([H|T], A1, A2, O) :-
    splitString(T, [H|A1], A2, O).
    
    
readQoutes([], A1, A2, O) :-
    splitString([], A1, A2, O).
readQoutes([H|T], A1, A2, O) :-
    H == 34, %! Qoute sign
    splitString(T, [H|A1], A2, O). %! Done reading quotes
readQoutes([H|T], A1, A2, O) :-
    readQoutes(T, [H|A1], A2, O).


readComments([], A1, A2, O) :-
    reverse(A1, O1),
    %!    A3 = [(com,O1)|A2],
    reverse(A2, O).
readComments([H|T], A1, A2, O) :-
    readComments(T, [H|A1], A2, O).

%! Input: String, Accumulator, word output, rest of the list output
splitStringTemporary([], A, O, []) :-
    reverse(A, A1),
    atom_codes(V1, A1),
    O = (temp, V1).
splitStringTemporary([H|T], A, O, [H|T]) :-
    H == 44, 
    reverse(A, A1), %! Reverse the accumulator
    atom_codes(V1, A1), %! transform string to an atomcode
    O = (temp, V1). %! set the output
splitStringTemporary([H|T], A, O, [H|T]) :-
    H == 32,
    reverse(A, A1),
    atom_codes(V1, A1),
    O = (temp, V1).
splitStringTemporary([H|T], A, O, G) :-
    splitStringTemporary(T, [H|A], O, G).

checkSection(L, [H|T], A2, O) :-
    H == 58,
    reverse(T, Reverse),
    atom_codes(OrdAtom, Reverse),
    splitString(L, [], [(section, OrdAtom)|A2], O).
checkSection(L, A1, A2, O)
    reverse(A1, O1),
    atom_codes(OrdAtom, O1),
    splitString(L, [], [(ord, OrdAtom)|A2], O).
    %! A3 = [(ord,OrdAtom)|A2],
    %! reverse(A3, O).

getAtomics([], []).
getAtomics([H|T], K) :- H == "", getAtomics(T, K).
getAtomics([H|T], K) :- getAtomics(T, K1), atom_codes(H1, H), append([H1], K1, K).



writeToFile(Lines) :-
    open('output.txt',write,OS), %! open the file
    writeToFile(Lines, OS).

writeToFile([], OS) :-
    close(OS).
writeToFile([H|T], OS) :-
    writeToFileLine(H, OS),
    write(OS, "\n"), %! add a new line
    writeToFile(T, OS).
    
    
writeToFileLine([], OS).
writeToFileLine([(temp,H)|T], OS) :-
    write(OS, H),
    writeToFileLine(T, OS).
writeToFileLine([(A,H)|T], OS) :-
    string_to_list(String, H),
    write(OS, String),
    writeToFileLine(T, OS).


tsplit(O) :-
    string_to_list("add %t1, %t2, %t3 ;hej", S),
    splitString(S, [], [], O).
    %! splitStringTemporary(S, [], O, T).