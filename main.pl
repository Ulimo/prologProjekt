:- use_module(readfile).
:- use_module(grammar).
:- use_module(liveness).
:- use_module(ranges).
:- use_module(graph).
:- use_module(colouring).
:- use_module(output).
:- use_module(spilling).

main(File, RegCount, O) :-
    readfile(File, OutputRead),!,
    allocateRegisters(OutputRead, RegCount, O).
    
allocateRegisters(OutputRead, RegCount, Output) :-
    grammar(OutputRead, OutputGrammar),
    buildLiveness(OutputGrammar, OutputLiveness),
    buildRanges(OutputLiveness, OutputRanges),
    createGraph(OutputRanges, OutputGraph),!,
    checkGraph(OutputGraph, RegCount, OutputRead, OutputLiveness, Output).
    
checkGraph(Graph, RegisterCount, Read, _, Output) :-
    removeUnderDegree(Graph, RegisterCount, SimplifiedGraph),
    colour(SimplifiedGraph, RegisterCount, _),
    colour(Graph, RegisterCount, Allocations),
    createOutput(Read, Allocations, "output.asm"),
    Output = Allocations.
    
checkGraph(Graph, RegisterCount, Read, Liveness, Output) :-
    removeUnderDegree(Graph, RegisterCount, SimplifiedGraph),
    spill(Liveness, SimplifiedGraph, Read, RegisterCount, O1),
    createOutput(O1, [], "output.asm"),
    allocateRegisters(O1, RegisterCount, Output).