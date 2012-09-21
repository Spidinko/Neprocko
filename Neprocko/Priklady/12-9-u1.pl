%% 12.9.12
%  prolog 1

%% stupne(+Hrany,-Stupne)
%  Jedna hrana = (a-b)

stupne(Edges, Degrees):-
	getVertices(Edges, [], Vertices),
	addDegrees(Vertices, Edges, Degrees).


addDegrees([], _, []).

addDegrees([V|Vs], E, [D|Ds]):-
	setDegree(V,E,0,D),
	addDegrees(Vs,E,Ds).


setDegree(V, [], A, (V,A)).

setDegree(V, [E|Es],A, D):-
	(((V-_) = E) ; ((_-V) = E)),
	!,
	A2 is A+1,
	setDegree(V,Es,A2,D).

setDegree(V, [_|Es], A, D):-
	setDegree(V,Es,A,D).


getVertices([], V, V).

getVertices([(A-B)|E], V, V_out):-
	add(A,V,V_tmp), add(B,V_tmp, V_acc),
	getVertices(E, V_acc, V_out).	

add(V, Vs, Vs):-
	member(V,Vs),
	!.

add(V, Vs, [V|Vs]).
	
