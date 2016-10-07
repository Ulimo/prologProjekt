:- use_module(readfile).
:- use_module(grammar).
:- use_module(liveness).
:- use_module(ranges).

main(O) :-
    readfile('ass.txt', OutputRead),
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, OutputLiveness),
    buildRanges(OutputLiveness, O).
    %! writeToFile(O).