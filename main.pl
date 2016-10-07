:- use_module(readfile).
:- use_module(grammar).
:- use_module(liveness).
:- use_module(ranges).
:- use_module(graph).
:- use_module(colouring).

main(O) :-
    readfile('ass.txt', OutputRead),
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, OutputLiveness),
    buildRanges(OutputLiveness, OutputRanges),
    createGraph(OutputRanges, OutputGraph),
    removeUnderDegree(OutputGraph, 3, SimplifiedGraph),!,
    colour(SimplifiedGraph, 3, O).
    %! writeToFile(O).