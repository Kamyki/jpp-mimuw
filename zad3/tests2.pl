%NOK
:- \+ jestEFGrafem([node(v4164, [], [v4188]), node(v4186, [], []),b]).

:- \+ jestEFGrafem([node(v4164, [], [v4188]), node(v4186, [], [])]).
:- \+ jestEFGrafem([node(v4164, [v4188], []), node(v4186, [], [])]).
:- \+ jestEFGrafem([node(v4164, [], []), node(v4186, [v4188], [])]).
:- \+ jestEFGrafem([node(v4164, [], [v4188]), node(v4186, [], [])]).
:- \+ jestEFGrafem([node(v4164, [], [v4186]), node(v4186, [], [v4188])]).

:- \+ jestEFGrafem([node(v4168, [], [v4182]), node(v4182, [], []), node(v4214, [], [])]).
:- \+ jestEFGrafem([node(v4168, [], [v4182]), node(v4182, [v4168], []), node(v4214, [], [])]).
:- \+ jestEFGrafem([node(v4168, [], [v4214,v4182]), node(v4182, [v4168], [v4168]), node(v4214, [], [])]).

:- \+ jestEFGrafem([node(v4168, [], []), node(v4182, [], [v4168]), node(v4214, [], [])]).

:- \+ jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [v704, v718])]).

:- \+ jestEFGrafem([node(v704, [], []), node(v704, [], []),node(v718, [v718, v718], [v704, v718])]).
:- \+ jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [v704, v718]),node(v718, [v718, v718], [v704, v718])]).

%NOK
:- \+ jestDobrzeUlozony([node(v1926, [v1940], []), node(v1940, [v1926, v1926],[])]).
:- \+ jestDobrzeUlozony([node(v988, [], []), node(v1002, [], []), node(v1016, [], []), node(v1030, [v988], [])]).
:- \+ jestDobrzeUlozony([node(v1926, [v1940], []), node(v1940, [v1926],[])]).
:- \+ jestDobrzeUlozony([node(v984, [v998,v1012], []), node(v998, [v1026], []), node(v1012, [v1026], []), node(v1026, [], [])]).
:- \+ jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026,v984], [v984]), node(v1012, [v1026], []), node(v1026, [], [v984])]).
:- \+ jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], []), node(v1026, [], [v984])]).

%NOK
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026], [v984]), node(v1012, [v1026], []), node(v1026, [], [v984])]).
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026], [v984]), node(v1012, [v1026,v998], []), node(v1026, [], [v984])]).
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], [v984]), node(v1026, [], [v984])]).
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], [v984]), node(v1026, [], [v984])]).



%NOK
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [], [v998])],[v984],[v984]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[v998,v1012]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v1012,v998],[v998,v1012,v1026]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[v998,v1026,v1012]).
