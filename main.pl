:- use_module(readfile2).
:- use_module(grammar2).
:- use_module(liveness).
:- use_module(ranges).
:- use_module(graph).
:- use_module(colouring).
:- use_module(output).

main(O) :-
    readfile('ass.txt', OutputRead),
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, OutputLiveness),
    buildRanges(OutputLiveness, OutputRanges),
    createGraph(OutputRanges, OutputGraph),!,
    %! removeUnderDegree(OutputGraph, 3, SimplifiedGraph),!,
    colour(OutputGraph, 4, Allocations),
    createOutput(OutputRead, Allocations, "output.asm").