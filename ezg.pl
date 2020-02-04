subset([], _).
subset([X|Y], Z) :- member(X, Z), subset(Y, Z).
member(X, [X|_]).
member(X, [_|Z]) :- member(X, Z).