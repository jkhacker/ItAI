h(17, 2).
h(1, 2).
o(19, 5).
o(18, 4).
o(17, 3).
o(16, 4).
o(16, 5).
o(16, 6).
o(15, 1).
o(15, 2).
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

in_list(L, X, Y) :-
	(
		[_|T] = L,
		in_list(T, X, Y)
	);
	(
		[L1|_] = L,
		L1 == [X, Y]
	).

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

start(Path) :-
	asserta(max(999)),
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
	Y #> Y1 -> write('v');(
		Y #< Y1 -> write('^');(
			X #> X1 -> write('<'); write('>')
		)
	).

print_row(X, Y, Path) :-
	(\+ x_in_bound(X), write('\n'));
	(x_in_bound(X), print_dot(X, Y, Path), write(' '), X1 is X+1, print_row(X1, Y, Path)).

print_map_(Y, Path) :-
	y_in_bound(Y),
	print_row(0, Y, Path),
	Y1 is Y - 1,
	print_map_(Y1, Path).

print_map(Path) :-
	print_map_(19, Path).
