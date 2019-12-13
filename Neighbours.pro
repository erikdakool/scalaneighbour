outputFile('./neighbours_solved.txt').
inputFile('./neighbours_unsolved.txt').

:- use_module(library(clpfd)).

possibleValues(Size, List):-
setof(Num, between(1,Size,Num),List).


checkLines([],_).
checkLines([H|T],Size):-permutation(H,Size),checkLines(T,Size).

permutation(_,0).
permutation(List,Size):-member(Size,List),
S1 is Size-1,
permutation(List, S1).


checkNeighbours(A,-,B,Size):-possibleValues(Size,ValueList),
                        member(A,ValueList),
                        member(B,ValueList),
                        Bp1 is B+1,Bm1 is B-1,
			\+member(A,[Bp1]),
			\+member(A,[Bm1]).

checkNeighbours(A,x,B,Size):-possibleValues(Size,ValueList),
member(A,ValueList), member(B,ValueList),  A is B+1,!.
checkNeighbours(A,x,B,Size):-possibleValues(Size,ValueList),
member(A,ValueList), member(B,ValueList),  A is B-1,!.


checkNeighbourLine([],[],_).
checkNeighbourLine([LH,LT],[N],Size):-checkNeighbours(LH,N,LT,Size).
checkNeighbourLine([LH,LM|LT],[NH|NT],Size):-checkNeighbours(LH,NH,LM,Size),
		       checkNeighbourLine([LM|LT],NT,Size).

checkNeighboursLines([],[],_).

checkNeighboursLines([SingleLine],[SingleNeighbourLine],Size):-
checkNeighbourLine(SingleLine,SingleNeighbourLine,Size).

checkNeighboursLines([LineHead|LineTail],[NeighbourLineHead|NeighbourLinetail],Size):-
checkNeighbourLine(LineHead,NeighbourLineHead,Size),
checkNeighboursLines(LineTail,NeighbourLinetail,Size).

extractNeighboursLine([],[]).
extractNeighboursLine([_],[]).
extractNeighboursLine([_,H|T],X):-
append([H],X2,X),
extractNeighboursLine(T,X2).

extractNeighboursBoard([],[]).
extractNeighboursBoard([Line],Extract):-
extractNeighboursLine(Line,ExtractedFromLine),
append([ExtractedFromLine],[],Extract).
extractNeighboursBoard([H,_|T],FullBoard):-
  extractNeighboursLine(H,Extracted),
  append([Extracted],NextLine,FullBoard),
  extractNeighboursBoard(T,NextLine).

extractValuesLine([],[]).
extractValuesLine([H],[H]).
extractValuesLine([H,_|T],[H|T1]):-
extractValuesLine(T,T1).

extractValuesBoard([],[]).
extractValuesBoard([Single],[Line]):-
extractValuesLine(Single,Line).
extractValuesBoard([H,_|T],Board):-
extractValuesLine(H,X),
extractValuesBoard(T,Board1),
append([X],Board1,Board).


prepareBoard(Size,Board):-
extractValuesBoard(Board,HorizontalValues),
transpose(Board,VerticalBoard),
extractValuesBoard(VerticalBoard,VerticalValues),
extractNeighboursBoard(Board,HorizontalNeighbours),
extractNeighboursBoard(VerticalBoard,VerticalNeighbours),
checkLines(VerticalValues,Size),
checkLines(HorizontalValues,Size),
checkNeighboursLines(HorizontalValues,HorizontalNeighbours,Size),
checkNeighboursLines(VerticalValues,VerticalNeighbours,Size),!.



doSolve(neighbours(size(Size),grid(Problem)),neighbours(size(Size),grid(Problem))):-
prepareBoard(Size,Problem).

removeNeighbours([P],[S]):- removeNeighbourLine(P,S).
removeNeighbours([P,_|PT],[S|ST]):- removeNeighbourLine(P,S), removeNeighbours(PT,ST).

removeNeighbourLine([P],[P]).
removeNeighbourLine([P,_|PT],[P|ST]):- removeNeighbourLine(PT,ST).

/********************* writing the result */
writeFullOutput(neighbours(size(N),grid(Grid))):-
  write('size '), write(N), write('x'), write(N), nl, writeGrid(Grid).

writeGrid([]).
writeGrid([E|R]):- writeGridLine(E), writeGrid(R).

writeGridLine([]):- nl.
writeGridLine([E|R]):- E='?', !, write(E), write(' '), writeGridLine(R).
writeGridLine([E|R]):- write(E), write(' '), writeGridLine(R).

/********************** reading the input */
readProblem(neighbours(size(N),grid(Grid))):-
  findKW(size), readInt(N), readInt(M), M=N, GridLength is N*2-1, length(Grid,GridLength),
  readGridLines(GridLength,Grid).

findKW(KW):- string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):- peek_code(-1), !, fail.
findKW(KW):- get_code(_), findKW(KW).

readKW([]):- get_code(_).
readKW([H|T]):- get_code(H), readKW(T).

readGridLines(N,[A]):- length(A,N), readGridLine(A).
readGridLines(N,[A,B|T]):- length(A,N), readGridLine(A), length(B,N), readNeighborLine(B), readGridLines(N,T).

readGridLine([N]):- readInt(I), makeHint(I,N).
readGridLine([N,X|T]):- readInt(I), makeHint(I,N), get_code(M), translate(M,X), !, readGridLine(T).

readNeighborLine([X]):- get_code(M), translate(M,X), !.
readNeighborLine([X,o|T]):- get_code(M), translate(M,X), get_code(_), get_code(_), get_code(_), !, readNeighborLine(T).

makeHint(X,X):- X>0.
makeHint(0,_).

translate(-1,'ERROR: EOF').
translate(120,'x').
translate(32,'-').
translate(X,X).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- string_codes(E,[X]).

whitespace(10). whitespace(12). whitespace(32).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.
is_number_code(95,0).

/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(P), doSolve(P, S), writeFullOutput(S), !, N1 is N-1, solveProblems(N1).

:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

:- run.
%:- halt.

