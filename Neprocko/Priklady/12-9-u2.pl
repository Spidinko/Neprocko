%% 12.9.2012
%% prolog 2

%% obdelniky(+Okno, +Obdelniky, -Zvysne_Obdelniky_Ktore_Sa_Pretinaju_S_Oknom)
%  format okna/obdelniku o(X1,Y1,X2,Y2)  vo vnutri su suradnice - lavo dole, pravo dole


obdelniky(_, [], []).

obdelniky(Win, [R|Rects], Ret):-
	outside(R,Win),
	!,
	obdelniky(Win, Rects, Ret).

obdelniky(Win, [R|Rects], [R|Ret]):-
	inside(R,Win),
	!,
	obdelniky(Win, Rects, Ret).

%  R pretina Win
obdelniky(Win, [R|Rects], [R_out|Ret]):-
	cutOff(Win,R,R_out),
	obdelniky(Win, Rects, Ret).

inside(o(X1,Y1,X2,Y2), o(A1,B1,A2,B2)):-
	X1>=A1, Y1>=B1, X2=<A2, Y2=<B2.		

outside(o(_,_,X2,_), o(A1,_,_,_)):-
	X2 =< A1.

outside(o(_,Y1,_,_), o(_,_,_,B2)):-
	Y1 >= B2.

outside(o(_,_,_,Y2), o(_,B1,_,_)):-
	Y2 =< B1.

outside(o(X1,_,_,_), o(_,_,A2,_)):-
	X1 >= A2.

cutOff(o(X1,Y1,X2,Y2), o(A1,B1,A2,B2), o(A,B,C,D)):-
	max(X1,A1,A), max(Y1,B1,B),
	min(X2,A2,C), min(Y2,B2,D).

max(X,Y,X):-
	X>=Y,
	!.

max(_,Y,Y).

min(X,Y,X):-
	X=<Y,
	!.

min(_,Y,Y).







