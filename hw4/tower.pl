tower(N, T, C) :- 
    lengthC(C, N),
    length(T, N), 
    maplist(checkLength(N), T),
    maplist(setDomain(N), T),
    maplist(fd_all_different, T),
    transpose(T, TT),
    maplist(fd_all_different, TT),
    maplist(fd_labeling, T),
    checkCounts(T, C).
    
checkLength(N, Row) :- 
    length(Row, N).

setDomain(N, Row) :- 
    fd_domain(Row, 1, N).

lengthC(counts(Top, Bottom, Left, Right), N) :-
    length(Top, N), length(Bottom, N), length(Left, N), length(Right, N).

checkCounts([], _, _). 
checkCounts(T, counts(Top, Bottom, Left, Right)) :- 
    transpose(T, TT), checkRow(TT, Top),
    reverselist(TT, RT), checkRow(RT, Bottom),
    checkRow(T, Left),
    reverselist(T, R), checkRow(R, Right).
    
count(Col, Acc) :-
  count(0, Col, 0, Acc).
count(_, [], Acc, Acc).
count(Max, [H|L], I, Acc) :-
  Max @< H, 
  I1 is I+1,
  count(H, L, I1, Acc), !.
count(Max, [H|L], I, Acc) :-
  Max @> H, count(Max, L, I, Acc).

checkRow([], []).
checkRow([Col|T], [C|Cs]) :- 
  count(Col, Acc), 
  C = Acc, 
  checkRow(T, Cs).

reverselist([], _).
reverselist([T|L], [RT|RL]) :-
  reverse(T, RT, []), reverselist(L, RL).

reverse([], Z, Z).
reverse([H|T], Z, Acc) :- reverse(T, Z, [H|Acc]).

% SWI-PROLOG clpfd transpose function used as tool

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

plain_tower(N, T, C) :- 
    lengthC(C, N),
    length(T, N), 
    maplist(checkLength(N), T),
    transpose(T, TT),
    checkgoodRows(N, T, TT),
    checkCounts(T, C).

checkgoodRows(_, [], []).
checkgoodRows(N, [H|T], [HH|TT]):-
  range(N, Tmp),
  permutation(Tmp, H),
  permutation(Tmp, HH),
  checkgoodRows(N, T, TT).

range(1, L) :- L = [1].
range(N, L):-
  N > 1,
  N1 is N - 1,
  range(N1, L1),
  append(L1, [N], L).

all_different([]).
all_different([H|T]):-
       \+member(H, T), all_different(T).

runtower(N, T, C, R) :- 
    statistics(cpu_time,[Start|_]),
    tower(N, T, C),
    statistics(cpu_time,[Stop|_]),
    R is Stop - Start,
    write('tower Execution took '), write(R), write(' ms.'), nl.

run_plaintower(N, T, C, R) :-
    statistics(cpu_time,[Start|_]),
    plain_tower(N, T, C),
    statistics(cpu_time,[Stop|_]),
    R is Stop - Start,
    write('plain tower Execution took '), write(R), write(' ms.'), nl.

speedup((N, T, C)) :-
    copy_term(N, N1),copy_term(T, T1),copy_term(C, C1),
    runtower(N, T, C, R),
    run_plaintower(N1, T1, C1, R1),
    write(R1),write(' '), write(R), nl,
    Output is R1/R, write(Output),nl.

speedup((5,[A,B,C,D,E], 
      counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2]))).

ambiguous(N, C, T1, T2) :-
  tower(N, T1, C), 
  tower(N, T2, C),
  compare(T1, T2).

compare(T1, T2):-
  same(T1, T2), !, fail. 
compare(_, _).

same([], []).
same([H1|T1], [H2|T2]):-
    sameRow(H1, H2), 
    same(T1, T2).

sameRow([], []).
sameRow([X|T1], [Y|T2]) :-
  X = Y, sameRow(T1, T2).