:- use_module(library(lists)) .


% Checks extensivly if atom comply with node representation
is_node(node(X, E, F)) :- 
    atom(X),
    is_list(E),
    is_list(F),
    forall(member(M, E), atom(M)),
    forall(member(M, E), atom(M)). 


% Checks if all atoms representing nodes are unique
is_unique_node_list([], V, R) :- is_set(V), R = V .
is_unique_node_list([X|Xs], V, R) :- is_node(X), arg(1, X, Z), is_unique_node_list(Xs, [Z|V], R) .
is_unique_node_list(X, R) :- is_unique_node_list(X, [], R) .


% Checks if all desination nodes exisits in Graph. It takes list of representing atoms instead of list of nodes 
destination_exists(_, [], [])  :- !.
destination_exists(V, [], P) :- subtract(P, V, []).
destination_exists(V, [node(_, E, F)|Nodes], P) :- 
    union(E,P,PP),
    union(F,PP,PPP),
    destination_exists(V, Nodes, PPP) .
destination_exists(V, Nodes) :- destination_exists(V, Nodes, []) .


% Checks if atom V exists in all nodes in Fedges list. It returns graph with removed Fedges that represented those connections. 
vertices_fedges_are_bidirectional(_, Graph, [], R) :- R = Graph , !.

vertices_fedges_are_bidirectional(V, Graph, [F|Fedges], R) :- 
    member(node(F, E1, Fedges2), Graph), !,
    select(V, Fedges2, Fedges3),
    select(node(F, E1, Fedges2), Graph, node(F, E1, Fedges3), Graph2),
    vertices_fedges_are_bidirectional(V, Graph2, Fedges, R) .

% Checks if all fedges in Graph are bidirectional
fedges_are_bidirectional(_, []) :- !. 
fedges_are_bidirectional(Graph, [node(V,_,Fedges)|N]) :-
     vertices_fedges_are_bidirectional(V, Graph, Fedges, R),
     fedges_are_bidirectional(R, N) .
fedges_are_bidirectional(Graph) :- fedges_are_bidirectional(Graph, Graph) .

convert_to_sets([], Graph, SetGraph) :- !, SetGraph = Graph .
convert_to_sets([node(V, Eedges, Fedges)|Graph], Converted, SetGraph) :- 
    list_to_set(Eedges, EedgesSet),
    list_to_set(Fedges, FedgesSet),
    convert_to_sets(Graph, [node(V, EedgesSet, FedgesSet)|Converted], SetGraph) .
convert_to_sets(Graph, SetGraph) :- convert_to_sets(Graph, [], SetGraph) .


jestEFGrafem(G) :- jestEFGrafem(G, _) .
jestEFGrafem(G, R) :- 
    convert_to_sets(G, Graph),
    is_unique_node_list(Graph, VerticeList),
    destination_exists(VerticeList, Graph),
    fedges_are_bidirectional(Graph), 
    R = VerticeList, !.


% Checks if number of fedges is less than 3 for each node
less_than_three(F) :- length(F, L), L =< 3 .
fedges_are_limited(Graph) :- 
    forall(member(node(_,_,F), Graph), less_than_three(F)), !.


% It finds unique source node
source(Graph, [node(V, E, F)|Ns], R) :- 
    select(X, E, E2),
    select(node(X, _, _), Graph, Graph2), !, 
    source(Graph2, [node(V, E2, F)|Ns], R) .
source([node(V, E, F)], [], R) :- 
    \+ member(V,E),
    R = node(V, E, F) , !.
source(Graph, [_|Ns], R) :- source(Graph, Ns, R) .
source(Graph, R) :- source(Graph, Graph, R) .


% It finds unique exit node
exit(Graph, R) :- 
    select(node(V, [], F), Graph, Graph2),
    \+ member(node(_, [], _), Graph2),
    R = node(V, [], F), ! .

% It Finds E-path which span whole graph, first it finds source and exit and then traverse graph.
% It keeps list of visited nodes and history of traversal, it backtrack if it finds itself in already visited state (set of visited nodes exists in history).
e_path(Graph, N, R) :- 
    source(Graph, S), % generate source
    exit(Graph, E),  % generate exit
    S \== E, !,
    arg(1, S, SS),
    arg(1, E, EE),
    e_path(Graph, state(E,N), state(S, [SS]), [h(SS, [SS])]), % find path
    R = (SS, EE) .
e_path(_, state(Ve, Goal), state(Ve, Visited), _) :- 
    subtract(Goal, Visited, []) . % check if we visited all graph
e_path(Graph, EndState, state(node(_, E, _),Visited), Path) :-
    member(V1, E), % generate possible extension of path
    member(node(V1, Eedges, Fedges), Graph),
    \+ member(h(V1,Visited), Path), % check if this extension is adding something to our serach, e.g. have we been in this state already?
    union([V1], Visited, NewVisited),
    e_path(Graph, EndState, state(node(V1, Eedges, Fedges), NewVisited) ,[h(V1,NewVisited)|Path]) .


jestDobrzeUlozony(Graph) :- jestDobrzeUlozony(Graph, _) .
    

jestDobrzeUlozony(G, R) :- 
    convert_to_sets(G, Graph),
    jestEFGrafem(Graph, V), 
    fedges_are_limited(Graph), 
    e_path(Graph, V, (S, E)),
    R = (S,E) . 


% Given nodes Z(V1) and W it find node X according to "dot one" from task
convert_one(node(_,_, FZ), node(_, EW, _)) :- 
    member(X, EW),
    member(X, FZ) .

% Given nodes Z(V1) and W it find node X according to "dot two" from task
convert_two(Graph, node(_,_, FZ), node(W, _, _)) :- 
    member(X, FZ),
    member(node(X, EX, _), Graph),
    member(W, EX) .

% Geneartes all possible Z(V1),W pair according to "dot two"
generate_two_candidate(Graph, Vsource, Z, W) :-
    member(node(XV, _, FV), Graph),
    member(XW, FV),
    XW \== Vsource,
    member(node(XW, EW, FW), Graph),
    member(node(XZ, EZ, FZ), Graph),
    member(XV, EZ),
    Z = node(XZ, EZ, FZ),
    W = node(XW, EW, FW) .

% Geneartes all possible Z(V1),W pair according to "dot one"
generate_one_candidate(Graph, Vexit, Z, W) :-
    member(node(_, EV, FV), Graph),
    member(XW, FV),
    XW \== Vexit,
    member(XZ, EV),
    member(node(XW, EW, FW), Graph),
    member(node(XZ, EZ, FZ), Graph),
    Z = node(XZ, EZ, FZ),
    W = node(XW, EW, FW) .

% Checks if graph is permutable, when given its exit and source
is_permutable(Graph, Vexit, Vsource) :-
    forall(generate_one_candidate(Graph, Vexit, Z, W), convert_one(Z, W)),
    forall(generate_two_candidate(Graph, Vsource, Z, W), convert_two(Graph, Z, W)) .


jestDobrzePermutujacy(Graph) :- jestDobrzePermutujacy(Graph, _) .

jestDobrzePermutujacy(G, R) :-
    convert_to_sets(G, Graph),
    jestDobrzeUlozony(Graph, (S, E)),
    is_permutable(Graph, E, S),
    R = (S, E) .



%e_conn(Graph, Path, Succ) 
% Check if Path is legal f-path in Graph and is of length L
is_f_path(_, [], L) :- L =:= 0 . 
is_f_path(Graph, [V], L) :- L =:= 1,  member(node(V, _, _), Graph) .   
is_f_path(Graph, [V, V1 | Path], L) :- 
    L > 0,
    member(node(V, _, FV), Graph),
    member(node(V1, _, _), Graph),
    member(V1, FV),
    is_f_path(Graph, [V1|Path], L-1) .


% Check e-connections in pair of Path and successor
e_conn(_, [], _) .

e_conn(Graph, [W|Ws], [V|Vs]) :-
    member(node(W, E, _), Graph),
    member(V, E),
    e_conn(Graph, Ws, Vs) .

% Get next number equal or greater than A
genNum(A,R):- R = A.
genNum(A,R):- succ(A,B), genNum(B, R).

% Get next number equal or greater than A, but not bigger than Limit
genNum(A,R,_):- R = A .
genNum(A,R,Limit):- A < Limit, succ(A,B), genNum(B, R, Limit).


jestSucc(Graph, Path, Succ) :-
    jestDobrzePermutujacy(Graph), % check graph
    genNum(0, L), % generate possible length for succesor
    is_f_path(Graph, Succ, L), % check successor
    genNum(0, LL, L), % generate possible length for path
    is_f_path(Graph, Path, LL), % check path
    e_conn(Graph, Path, Succ). % check e_connections in path-successor





