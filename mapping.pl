print_dot(X, Y, Path) :-
	o(X,Y) -> write('O'); (
		t(X,Y) -> write('T'); (
			in_list(Path, X, Y) -> print_arrow(X, Y, Path), !; (
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
	(X == X1 ->
		(Y > Y1 -> 
			(write('v'));
			(write('^'))
		);
		(Y == Y1 -> 
			(X > X1 -> 
				(write('<'));
				(write('>'))
			);
			(write('@'))
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