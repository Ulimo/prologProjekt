sum2([], 0).
sum2([H|T], S) :-
    S=H+V,
    sum2(T,V).
    

quicksort([X|Xs], Ys) :-
    partition(xs, X, Littles, Bigs),
    quicksort(Littles, Ls),
    quicksort(Bigs, Bs),
    append(Ls, [X|Bs], Ys).
quicksort([],[]).

partition([Y|Ys], X, [Y|Ls], Bs) :-
    X>Y, partition(Ys, X, LS, Bs).
    
append([], Xs, Xs).
append([X|Xs], Y, [X|Zs]) :- append(Xs, Y, Zs).


lookup(Key, tree(Key, Value, Left, Right), Value) :- !.
lookup(Key, tree(Key1, Value1, Left, Right), Value) :-
    Key < Key1, lookup(Key, Left, Value).
lookup(Key, tree(Key1, Value1, Left,Right, Value) :-
    Key > Key1, lookup(Key, Right, Value).

test([], 0).
test([H|T], S) :- test(T, S), S is H+S.

merge([H1,H2|T], S) :-
    H1 > H2, S is H1.
merge([H1,H2|T], S) :-
    H1 < H2, S is H2.
    
test3(klass1, func1, attr1).

minimum(X,Y,Z) :- X =< Y, !, Z=X.
minimum(X,Y,Y).


%! Condition, !, TruePart.
%!   ElsePart.
% Condition -> TruePart; ElsePart).

nand(1,1, 0).
nand(0,1, 1).
nand(1, 0, 1).
nand(0, 0, 1).

xor(A,B,S) :- nand(A,B,V1), nand(A,V1,V2), nand(B,V1,V3), nand(V2,V3,S).

and(A,B,S) :- nand(A,B,V1), nand(V1,V1,S).


sentence --> noun_phrase, verb_phrase.
noun_phrase --> [the], noun.
verb_phrase --> [runs].
noun --> [rabbit].
noun --> [engine].

expr(X) -->
    term(Y),
    [+],
    expr(Z),
    {X is Y + Z}.
expr(X) -->
    term(Y), 
    [-], expr(Z), 
    {X is Y-Z}.
expr(X) --> 
    term(X).

term(X) --> 
    factor(Y), 
    [*], 
    term(Z), 
    {X is Y*Z}.
term(X) --> 
    factor(Y),
    [/], 
    term(Z), 
    {X is Y/Z}.
term(X) --> 
    factor(X).
factor(X) --> 
    [X], 
    {integer(X)}.


%! expr(X, [2, *, 2, +, 4, *, 4], []).


test(O) :-
    O = alex.
    
    
gener([],_,[]).
gener([V|Vs],Colors,[V-C|T]):-
   member(C,Colors), % non-deterministic generator of colors
   gener(Vs,Colors,T).
    
    
    
findRangeNew([], Var, Index, 0, A, Output) :-
    reverse(A, Output).
findRangeNew([], Var, Index, 1, A, Output) :-
    Old is Index - 1,
    reverse([Old|A], Output).
findRangeNew([H|T], Var, Index, 0, A, Output) :-
    memberchk((_, Var), H),
    K is Index + 1,
    findRangeNew(T, Var, K, 1, [Index|A], Output).
findRangeNew([H|T], Var, Index, 1, A, Output) :-
    memberchk((s, Var), H),
    K is Index + 1,
    findRangeNew(T, Var, K, 1, A, Output).
findRangeNew([H|T], Var, Index, 1, A, Output) :-
    memberchk((d, Var), H),
    Old is Index - 1,
    K is Index + 1,
    findRangeNew(T, Var, K, 1, [Index,Old|A], Output).
findRangeNew([H|T], Var, Index, 1, A, Output) :-
    Old is Index - 1,
    K is Index + 1,
    findRangeNew(T, Var, K, 0, [Old|A], Output).
findRangeNew([H|T], Var, Index, 0, A, Output) :- %! No variable here, continue to iterate
    K is Index + 1,
    findRangeNew(T, Var, K, 0, A, Output).