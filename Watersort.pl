
:- dynamic can_eval/1.
:- dynamic state/4.
:- dynamic bottle1/2.
:- dynamic bottle2/2.
:- dynamic bottle3/2.
:- dynamic step/2.
:- dynamic pour/2.
step(s0,0).


% Define s0 to encapsulate the knowledge base as a single symbol.
initial :-
    findall(bottle1(C1, C2), bottle1(C1, C2), B1),
    findall(bottle2(C3, C4), bottle2(C3, C4), B2),
    findall(bottle3(C5, C6), bottle3(C5, C6), B3),
    assert(state(B1,B2,B3,0)).




valid_pour(A, B) :-
    member((A, B), [(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)]).


pair((1, 2)).
pair((1, 3)).
pair((2, 1)).
pair((2, 3)).
pair((3, 1)).
pair((3, 2)).


resultH(pour(A, B), S) :-
    state([bottle1(C11, C12)], [bottle2(C21, C22)], [bottle3(C31, C32)], S), (

    % transfer from 1 to 2
    (A = 1, B = 2, ( 
    (C11 \= e, C22 = e, C11 \= C12, assert(state([bottle1(e, C12)], [bottle2(C21, C11)], [bottle3(C31, C32)], S+1))); 
    (C11 \= e, C22 \= e, C21 = e, C11 = C22, C11 \= C12, assert(state([bottle1(e, C12)], [bottle2(C11, C22)], [bottle3(C31, C32)], S+1))); 
    (C11 = e, C12 \= e, C22 = e, assert(state([bottle1(e, e)], [bottle2(C21, C12)], [bottle3(C31, C32)], S+1))); 
    (C11 = e, C12 \= e, C21 = e, C22 \= e, C12 = C22,  assert(state([bottle1(e, e)], [bottle2(C12, C22)], [bottle3(C31, C32)], S+1))))); 

    % transfer from 1 to 3
    (A = 1, B = 3,( 
    (C11 \= e, C32 = e, C11 \= C12, assert(state([bottle1(e, C12)], [bottle2(C21, C22)], [bottle3(C31, C11)], S+1))); 
    (C11 \= e, C32 \= e, C31 = e, C11 = C32, C11 \= C12, assert(state([bottle1(e, C12)], [bottle2(C21, C22)], [bottle3(C11, C32)], S+1))); 
    (C11 = e, C12 \= e, C32 = e, assert(state([bottle1(e, e)], [bottle2(C21, C22)], [bottle3(C31, C12)], S+1))); 
    (C11 = e, C12 \= e, C31 = e, C32 \= e, C12 = C32,  assert(state([bottle1(e, e)], [bottle2(C21, C22)], [bottle3(C12, C32)], S+1)))));
    
    % transfer from 2 to 1
    (A = 2, B = 1,( 
    (C21 \= e, C12 = e, C21 \= C22, assert(state([bottle1(C11, C21)], [bottle2(e, C22)], [bottle3(C31, C32)], S+1))); 
    (C21 \= e, C12 \= e, C11 = e, C21 = C12, C21 \= C22, assert(state([bottle1(C21, C12)], [bottle2(e, C22)], [bottle3(C31, C32)], S+1))); 
    (C21 = e, C22 \= e, C12 = e, assert(state([bottle1(C11, C22)], [bottle2(e, e)], [bottle3(C31, C32)], S+1))); 
    (C21 = e, C22 \= e, C11 = e, C12 \= e, C22 = C12,  assert(state([bottle1(C22, C12)], [bottle2(e, e)], [bottle3(C31, C32)], S+1)))));
    
    % transfer from 2 to 3
    (A = 2, B = 3,( 
    (C21 \= e, C32 = e, C21 \= C22, assert(state([bottle1(C11, C12)], [bottle2(e, C22)], [bottle3(C31, C21)], S+1))); 
    (C21 \= e, C32 \= e, C31 = e, C21 = C32, C21 \= C22, assert(state([bottle1(C11, C12)], [bottle2(e, C22)], [bottle3(C21, C32)], S+1))); 
    (C21 = e, C22 \= e, C32 = e, assert(state([bottle1(C11, C12)], [bottle2(e, e)], [bottle3(C31, C22)], S+1))); 
    (C21 = e, C22 \= e, C31 = e, C32 \= e, C22 = C32,  assert(state([bottle1(C11, C12)], [bottle2(e, e)], [bottle3(C22, C32)], S+1)))));
    
    % Transfer from 3 to 1
    (A = 3, B = 1, (
        (C31 \= e, C12 = e, C31 \= C32, assert(state([bottle1(C11, C31)], [bottle2(C21, C22)], [bottle3(e, C32)], S+1)));
        (C31 \= e, C12 \= e, C11 = e, C31 = C12, C31 \= C32, assert(state([bottle1(C31, C12)], [bottle2(C21, C22)], [bottle3(e, C32)], S+1)));
        (C31 = e, C32 \= e, C12 = e, assert(state([bottle1(C11, C32)], [bottle2(C21, C22)], [bottle3(e, e)], S+1)));
        (C31 = e, C32 \= e, C11 = e, C12 \= e, C32 = C12, assert(state([bottle1(C32, C12)], [bottle2(C21, C22)], [bottle3(e, e)], S+1)))));

    % Transfer from 3 to 2
    (A = 3, B = 2, (
        (C31 \= e, C22 = e, C31 \= C32, assert(state([bottle1(C11, C12)], [bottle2(C21, C31)], [bottle3(e, C32)], S+1)));
        (C31 \= e, C22 \= e, C21 = e, C31 = C22, C31 \= C32, assert(state([bottle1(C11, C12)], [bottle2(C31, C22)], [bottle3(e, C32)], S+1)));
        (C31 = e, C32 \= e, C22 = e, assert(state([bottle1(C11, C12)], [bottle2(C21, C32)], [bottle3(e, e)], S+1)));
        (C31 = e, C32 \= e, C21 = e, C22 \= e, C32 = C22, assert(state([bottle1(C11, C12)], [bottle2(C32, C22)], [bottle3(e, e)], S+1)))))).






result(Action, result(Action2, S)) :-
    result(Action2, S),
    step(S, N),
    findall(P, pair(P), L),
    useHelper(Action, N + 1, L),
    assert(step(result(Action2, S), N+1)).

result(Action, s0) :-
    initial,
    findall(P, pair(P), L),
    useHelper(Action, 0, L).


useHelper(pour(A,B), N, [H | L]) :-
    (A,B) = H, resultH(pour(A,B), N), !; useHelper(pour(A,B), N, L).



goal(s0) :-
    initial,
    state([bottle1(A, B)], [bottle2(C, D)], [bottle3(E, F)], 0),
    A = B, C = D, E = F.

goal(S) :-
    step(S, N),
    state([bottle1(A, B)], [bottle2(C, D)], [bottle3(E, F)], N),
    A = B, C = D, E = F.

goal(result(Action, S)) :-
    result(Action, S),
    step(S, N),
    assert(step(result(Action, S), N+1)),
    goal(result(Action, S)).



   

ids(X, L) :- 
    call_with_depth_limit(goal(X), L, R), 
    number(R), !. % Found solution.
ids(X, L) :- 
    call_with_depth_limit(goal(X), L, R), 
    R = depth_limit_exceeded, 
    L1 is L + 1, 
    ids(X, L1).