:- use_module(readfile).

main(O) :-
    readfile('ass.txt', O1),
    reverse(O1, O).
    %! writeToFile(O).