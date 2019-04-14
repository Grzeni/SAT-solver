%definiujemy modul zawierajacy rozwiazanie
:-module(grzegorz_winiarski, [solve/2]).

%definiujemy operatory
:-op(200, fx, ~).
:-op(500, xfy, v).

in_clause(X,L):-
	is_variable(X),
	non_empty_clause(L),
	X = L.
in_clause(X,L v C):-
	is_variable(X),
	non_empty_clause(L v C),
	X = L,!;
	in_clause(X,C),
	!.
in_clause2(X,L):-
	is_variable(X),
	non_empty_clause(L),
	X = A,
	is_variable(A),
	~A = L.

in_clause2(X,L v C):-
	is_variable(X),
	non_empty_clause(L v C),
	X = A,
	is_variable(A),
	~A = L,!;
	in_clause2(X,C).


variables_in_clause(A,[A]):-
	is_variable(A).
variables_in_clause(~A,[A]):-
	is_variable(A).
variables_in_clause(L v C,List):-
	non_empty_clause(L v C),
	L = ~A,
	!,
	is_variable(A),
	List=[A|List1],
	variables_in_clause(C,List1).
variables_in_clause(L v C,List):-
	non_empty_clause(L v C),
	List=[L|List1],
	variables_in_clause(C,List1).

variables_in_set([],[]).
variables_in_set([H|T],SortedList):-
	variables_in_clause(H,L1),
	variables_in_set(T,L2),
	append(L1,L2,List),
	sort(List,SortedList).

remove(_,[],[]):-!.
remove(X,[H|T],NewSet):-
	non_empty_clause(H),
	in_clause(X,H),
	!,
	remove(X,T,NewSet).
remove(X,[H|T],[H|NewSet]):-
	non_empty_clause(H),
	remove(X,T,NewSet).

remove_false(_,[],[]):-!.
remove_false(X,[H|T],NewSet):-
	non_empty_clause(H),
	in_clause2(X,H),!,
	remove_false(X,T,NewSet).
remove_false(X,[H|T],[H|NewSet]):-
	non_empty_clause(H),
	remove_false(X,T,NewSet).


solve([],[],[]).
solve([K|T],[Z|T1],[(Z,t)|Solution]):-
	remove(Z,[K|T],NewSet),
	solve(NewSet,T1,Solution).
solve([K|T],[Z|T1],[(Z,f)|Solution]):-
	remove_false(Z,[K|T],NewSet),
	solve(NewSet, T1, Solution).
solve([],[Z|T1],[(Z,x)|Solution]):-
	solve([],T1,Solution).
solve(Clauses,Solution):-
	variables_in_set(Clauses,Set),
	solve(Clauses,Set,Solution).



is_variable([]):-!,fail.
is_variable(X):-atom(X).

literal(X):-is_variable(X),!.
literal(~X):-is_variable(X).

non_empty_clause(L):- literal(L),!.
non_empty_clause(L v C):- literal(L), non_empty_clause(C).

clause([]):-!.
clause(C):-non_empty_clause(C).

logical_value((X,V)):-
    is_variable(X),member(V,[t,f]).

generic_logical_value((X,V)):-
    is_variable(X),member(V,[t,f,x]).



