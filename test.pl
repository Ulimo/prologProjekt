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


