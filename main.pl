:- use_module(readfile).
:- use_module(grammar).

main(O) :-
    readfile('ass.txt', O1),
    grammar(O1, O).
    %! writeToFile(O).