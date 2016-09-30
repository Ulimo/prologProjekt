:- use_module(readfile).
:- use_module(grammar).
:- use_module(liveness).

main(O) :-
    readfile('ass.txt', OutputRead),
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, O).
    
    %! writeToFile(O).