h(17, 2).
h(1, 2).
o(19, 5).
o(18, 4).
o(17, 3).
o(16, 2).
o(16, 4).
o(16, 5).
o(16, 6).
o(15, 1).
o(15, 2).
o(15, 3).
o(15, 6).
o(14, 0).
o(14, 2).
o(14, 6).
o(13, 2).
o(13, 4).
o(13, 5).
o(13, 6).
o(12, 2).
o(12, 4).
o(11, 0).
o(11, 1).
o(11, 2).
o(11, 4).
o(10, 4).
o(9, 1).
o(9, 2).
o(9, 3).
o(9, 4).
o(8, 1).
o(7, 1).
o(6, 1).
o(5, 1).
o(4, 1).
o(3, 1).
o(2, 1).
o(2, 2).
o(1, 3).
o(0, 1).
o(0, 2).
t(18, 1).
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

start(Path) :-
	move(0, 0, [], Path),
	max(N),
	length(Path, N1),
	(N > N1 -> retractall(max(_)), asserta(max(N1)); true).


print_dot(X, Y, Path) :-
	o(X,Y) -> write('O'); (
		t(X,Y) -> write('T'); (
			in_list(Path, X, Y) -> print_arrow(X, Y, Path); (
				h(X, Y) -> write('H');
				write('*')
			)
		)
	).

find_next(X, Y, X1, Y1, [[X2,Y2]|T]) :-
	find_next(X, Y, X1, Y1, T);
	(X == X2, Y == Y2, [[X1, Y1]|_] = T).

print_arrow(X, Y, Path) :-
	find_next(X, Y, X1, Y1, Path),
	coord(X1, Y1),
	(X == X1) -
	(Y #> Y1, X == X1)-> (write('v'), !);(
		(Y #< Y1, X == X1) -> (write('^'), !);(
			(X #> X1, Y == Y) -> write('<'); write('>')
		)
	).

print_row(X, Y, Path) :-
	(\+ x_in_bound(X), write('\n'));
	(x_in_bound(X), print_dot(X, Y, Path), write(' '), X1 is X+1, print_row(X1, Y, Path)).

print_ux_indecies(X) :-
	x_in_bound(X),
	T is X//10,
	(X < 10 -> write(' '); write(T)),
	write(' '),
	X1 is X + 1,
	print_ux_indecies(X1).

print_bx_indecies(X) :-
	x_in_bound(X),
	T is X mod 10,
	write(T),
	write(' '),
	X1 is X + 1,
	print_bx_indecies(X1).

print_map_(Y, Path) :-
	y_in_bound(Y),
	write(Y),
	write('  '),
	(Y < 10 -> write(' '); true),
	print_row(0, Y, Path),
	Y1 is Y - 1,
	print_map_(Y1, Path).

print_map(Path) :-
	print_map_(19, Path);
	write('y/x '),
	print_ux_indecies(0);
	write('\n    '),
	print_bx_indecies(0).
