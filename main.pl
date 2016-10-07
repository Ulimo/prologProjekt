:- use_module(readfile).
:- use_module(grammar).
:- use_module(liveness).
:- use_module(ranges).
:- use_module(graph).

main(O) :-
    readfile('ass.txt', OutputRead),
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, OutputLiveness),
    buildRanges(OutputLiveness, OutputRanges),
    createGraph(OutputRanges, OutputGraph),
    removeUnderDegree(OutputGraph, 4, O).
    %! writeToFile(O).