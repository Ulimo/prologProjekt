:- use_module(readfile2).
:- use_module(grammar).
:- use_module(liveness).
:- use_module(ranges).
:- use_module(graph).
:- use_module(colouring).
:- use_module(output).
:- use_module(spilling).

main(O) :-
    readfile('ass.txt', OutputRead),!,
    allocateRegisters(OutputRead, O).
    %! grammar(OutputRead, OutputGrammar),
    %! buildLiveness(OutputGrammar, OutputLiveness).
    %! buildRanges(OutputLiveness, OutputRanges),
    %! createGraph(OutputRanges, OutputGraph),!,
    %! colour(OutputGraph, 3, O).
    %! spill(OutputLiveness, OutputGraph, OutputRead, OutputSpill),!,
    %! createOutput(OutputSpill, [], "output1.asm"),
    %! O = OutputSpill.
    %! removeUnderDegree(OutputGraph, 3, O).
    %!,
    %! createOutput(OutputRead, Allocations, "output.asm").
    %! colour(OutputGraph, 4, O).
    
allocateRegisters(OutputRead, Output) :-
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, OutputLiveness),
    buildRanges(OutputLiveness, OutputRanges),
    createGraph(OutputRanges, OutputGraph),!,
    checkGraph(OutputGraph, 3, OutputRead, OutputLiveness, Output).
    
checkGraph(Graph, RegisterCount, Read, _, Output) :-
    removeUnderDegree(Graph, RegisterCount, SimplifiedGraph),
    colour(SimplifiedGraph, RegisterCount, O1),
    colour(Graph, RegisterCount, Allocations),
    createOutput(Read, Allocations, "output.asm"),
    O = Allocations.
checkGraph(Graph, RegisterCount, Read, Liveness, Output) :-
    removeUnderDegree(Graph, RegisterCount, SimplifiedGraph),
    spill(Liveness, SimplifiedGraph, Read, O1),
    createOutput(O1, [], "output.asm"),
    allocateRegisters(O1, Output).