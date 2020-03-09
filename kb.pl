h(16, 18).
h(14, 8).
h(14, 9).
h(14, 10).
h(14, 11).
h(14, 12).
h(14, 13).
h(14, 14).
h(14, 15).
h(13, 8).
h(13, 13).
h(13, 14).
h(13, 15).
h(12, 15).
h(12, 16).
h(11, 15).
h(11, 16).
h(11, 17).
h(10, 14).
h(10, 16).
h(10, 17).
h(9, 5).
h(9, 9).
h(9, 11).
h(9, 15).
h(9, 17).
h(8, 14).
h(8, 15).
h(8, 16).
h(8, 17).
h(7, 14).
h(7, 15).
h(7, 16).
h(7, 17).
h(6, 15).
h(6, 17).
h(6, 18).
h(5, 16).
h(5, 17).
h(3, 9).
h(1, 13).
h(0, 13).
o(19, 7).
o(18, 3).
o(18, 4).
o(18, 10).
o(18, 12).
o(18, 14).
o(18, 17).
o(17, 1).
o(17, 2).
o(17, 3).
o(17, 5).
o(17, 8).
o(17, 13).
o(17, 15).
o(16, 0).
o(16, 1).
o(16, 2).
o(16, 4).
o(16, 5).
o(16, 10).
o(16, 12).
o(16, 15).
o(16, 17).
o(15, 8).
o(15, 14).
o(14, 1).
o(14, 3).
o(14, 5).
o(14, 7).
o(14, 18).
o(13, 1).
o(13, 2).
o(13, 6).
o(13, 9).
o(13, 10).
o(13, 11).
o(13, 12).
o(13, 16).
o(13, 17).
o(12, 1).
o(12, 5).
o(12, 8).
o(12, 13).
o(12, 14).
o(12, 17).
o(11, 7).
o(11, 11).
o(11, 14).
o(11, 18).
o(10, 2).
o(10, 4).
o(10, 7).
o(10, 10).
o(10, 12).
o(10, 15).
o(9, 3).
o(9, 7).
o(9, 14).
o(9, 16).
o(9, 18).
o(8, 6).
o(8, 8).
o(8, 13).
o(8, 18).
o(8, 19).
o(7, 1).
o(7, 2).
o(7, 4).
o(7, 9).
o(7, 10).
o(7, 12).
o(7, 13).
o(6, 0).
o(6, 3).
o(6, 5).
o(6, 7).
o(6, 10).
o(6, 12).
o(6, 14).
o(6, 16).
o(5, 2).
o(5, 6).
o(5, 8).
o(5, 10).
o(5, 15).
o(4, 1).
o(4, 3).
o(4, 10).
o(4, 12).
o(4, 13).
o(4, 15).
o(4, 18).
o(3, 7).
o(3, 10).
o(3, 12).
o(3, 13).
o(3, 17).
o(2, 0).
o(2, 3).
o(2, 5).
o(2, 8).
o(2, 10).
o(2, 13).
o(2, 16).
o(1, 4).
o(1, 7).
o(1, 11).
o(1, 15).
o(1, 18).
o(1, 19).
o(0, 1).
o(0, 3).
o(0, 7).
o(0, 10).
o(0, 12).
t(10, 11).
dynamic(max/1).
direction(0, 1).
direction(0, -1).
direction(1, 0).
direction(1, 1).
direction(1, -1).
direction(-1, 0).
direction(-1, 1).
direction(-1, -1).

x_in_bound(X) :-
	X < 20,
	X >= 0.

y_in_bound(Y) :-
	Y < 20,
	Y >= 0.

coord(X, Y) :-
	write(X),
	write(' '),
	write(Y),
	write('\n').

in_bound(X, Y) :-
	x_in_bound(X),
	y_in_bound(Y).

neighbour(X, Y, X1, Y1, Visited) :-
	(
		(X1 is X + 1; X1 is X - 1),
		Y1 is Y,
		check(X1, Y1, Visited)
	);
	(
		(Y1 is Y + 1; Y1 is Y - 1),
		X1 is X,
		check(X1, Y1, Visited)
	).

check(X, Y, Visited) :-
	in_bound(X, Y),
	\+ o(X, Y),
	\+ in_list(Visited, X, Y).

get_neighbour(X, Y, X1, Y1, Visited) :-
	neighbour(X, Y, X_, Y_, Visited),
	h(X_, Y_),
	get_neighbour(X_, Y_, X1, Y1, [[X,Y]|Visited]).

get_neighbour(X, Y, X1, Y1, Visited) :-
	neighbour(X, Y, X1, Y1, Visited),
	\+ h(X1, Y1).

get_neighbour_(X, Y, L, Visited) :-
	get_neighbour(X, Y, X1, Y1, Visited),
	L = [X1, Y1].

get_pass_(X, Y, L, Visited) :-
	pass(X, Y, X1, Y1, Visited),
	L = [X1, Y1].

get_states(X, Y, States, Visited) :-
	findall(L, (get_neighbour_(X, Y, L, Visited);get_pass_(X, Y, L, Visited)), States).

pick_random(X, Y, State) :-
	get_states(X, Y, States, []),
    length(States, Length),
    random(0, Length, Index),
    nth0(Index, States, State).

random_move(X, Y, NumOfMoves, Path) :-
	t(X,Y) -> Path = [[X, Y]],!;
	(
		NumOfMoves > 0,
		pick_random(X, Y, State),
		[X1,Y1] = State,
		N1 is NumOfMoves - 1,
		random_move(X1, Y1, N1, P),
		Path = [[X, Y]|P]
	).

rm(Path) :-
	random_move(0, 0, 100, Path).

random_start_(Path, N, Len) :-
	N > 0,
	N1 is N - 1,
	format('Current attempt: ~d ~n', [N]),
	(rm(P) -> (
			length(P, PLen),
			(PLen < Len -> 
				(
					random_start_(P1, N1, PLen), 
					length(P1, L1),
					(PLen < L1 ->
						Path = P;
						(L1 > 0 -> Path = P1; Path = P)
					)
				);
				(
					random_start_(P1, N1, Len),
					Path = P1
				)
			)
		);
		(
			random_start_(P1, N1, Len),
			Path = P1
		)
	).

random_start_([], 0, _).

random_start(Path) :-
	random_start_(Path, 100, 400).

in_list(L, X, Y) :-
	(
		[_|T] = L,
		in_list(T, X, Y)
	);
	(
		[L1|_] = L,
		L1 == [X, Y]
	).

abs(I, Res) :-
	I < 0 -> Res is -I; Res is I.

heur(X, Y, Res) :-
	t(Xt, Yt),
	abs((X-Xt), Xd),
	abs((Y-Yt), Yd),
	Res is 10*(Xd + Yd).

get_min_heur_([[X, Y]| Tail], MinHeur, ResCoords) :-
	heur(X, Y, ResHeur),
	(ResHeur < MinHeur -> (
		get_min_heur_(Tail, ResHeur, ResCoords1),
		[X1, Y1] = ResCoords1,
		heur(X1, Y1, R1),
		(ResHeur < R1 -> ResCoords = [X, Y], !; ResCoords = ResCoords1)
	);
	get_min_heur_(Tail, MinHeur, ResCoords)).

get_min_heur_([[X,Y]], _, ResCoords) :-
	ResCoords = [X, Y].

get_min_heur(Coords, ResCoords) :-
	get_min_heur_(Coords, 400, ResCoords).

filter(_, [], []).
filter(El, [E|Es], Res) :-
   (El \== E *-> Res = [E|Fs]; Res = Fs),
   filter(El, Es, Fs).

greedy_move(X, Y, Visited, Path) :-
	t(X,Y) -> Path = [[X, Y]],!; (
		get_states(X, Y, States, Visited),
		greedy_move_(States, [X1, Y1]),
		V1 = [[X, Y]|Visited],
		greedy_move(X1, Y1, V1, P1),
		Path = [[X, Y]|P1]
	).

greedy_move_(States, State) :-
	get_min_heur(States, NextCoord),
	filter(NextCoord, States, FilteredStates),
	(State = NextCoord; greedy_move_(FilteredStates, State)).

greedy_start(Path) :-
	greedy_move(0, 0, [], Path).

pass(X, Y, X1, Y1, Visited) :-
	direction(Xd, Yd),
	pass_(X, Y, X1, Y1, Visited, Xd, Yd).

pass_(X, Y, X1, Y1, Visited, Xd, Yd) :-
	X1 is X + Xd,
	Y1 is Y + Yd,
	h(X1, Y1),
	\+ in_list(Visited, X1, Y1).

pass_(X, Y, X1, Y1, Visited, Xd, Yd) :-
	Xt is X + Xd,
	Yt is Y + Yd,
	\+ o(Xt, Yt),
	in_bound(Xt, Yt),
	pass_(Xt, Yt, X1, Y1, Visited, Xd, Yd).

move(X, Y, Visited, Path) :-
	t(X,Y) -> Path = [[X, Y]],!;(
		get_neighbour(X, Y, X1, Y1, Visited),
		V1 = [[X, Y]|Visited],
		length(V1, N1),
		max(N),
		N1 < N,
		move(X1, Y1, V1, P1),
		Path = [[X, Y]|P1]
	).

move(X, Y, Visited, Path) :-
	t(X,Y) -> Path = [[X, Y]],!;(
		pass(X, Y, X1, Y1, Visited),
		V1 = [[X, Y]|Visited],
		length(V1, N1),
		max(N),
		N1 < N,
		move(X1, Y1, V1, P1),
		Path = [[X, Y]|P1]
	).

start_(Path) :-
	asserta(max(400)),
	move(0, 0, [], Path),
	max(N),
	length(Path, N1),
	(N > N1 -> retractall(max(_)), asserta(max(N1)); true).
