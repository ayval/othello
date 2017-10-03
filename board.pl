

%init the board with 4 pieces
init_board(Board):-
	Board = [
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,black,white,empty,empty,empty],
		[empty,empty,empty,white,black,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty]
	].
getCell(Board,X,Y,Cell):-
	nth0(Y,Board,Row),
	nth0(X,Row,Cell).


opponent(white,black).
opponent(black,white).


setItemInList([_|RestOfList],0,NewValue,[NewValue|RestOfList]).

setItemInList([Head|RestOfList],X,NewValue,[Head|NewRestOfList]):-
	NewX is X-1,
	setItemInList(RestOfList,NewX,NewValue,NewRestOfList),!.


setCell(Board,X,Y,NewValue,NewBoard):-
	nth0(Y,Board,Row),
	setItemInList(Row,X,NewValue,NewRow),
	setItemInList(Board,Y,NewRow,NewBoard).

countValueInRow(Row,Value,ReturnCount):-
	countValueInRow(Row,Value,0,ReturnCount).

countValueInRow([],_,Count,Count).

countValueInRow([X|Rest],Value,Count,ReturnCount):-
	(X=Value->
		NewCount is Count+1;
		NewCount is Count
	),
	countValueInRow(Rest,Value,NewCount,ReturnCount).

countSumOfRows(Board,Value,ReturnCount):-
	countSumOfRows(Board,Value,0,ReturnCount).

countSumOfRows([],_,Count,Count).

countSumOfRows([Row|RestOfBoard],Value,Count,ReturnCount):-
	countValueInRow(Row,Value,TempCount),
	NewCount is Count+TempCount,
	countSumOfRows(RestOfBoard,Value,NewCount,ReturnCount).

countBlacks(Board,Count):-
	countSumOfRows(Board,black,Count).

countWhites(Board,Count):-
	countSumOfRows(Board,white,Count).

countEmpty(Board,Count):-
		countSumOfRows(Board,empty,Count).

boardIsFull(Board):-
	countEmpty(Board,Count),
	Count=0.

printRow([]):-
	writeln('').

printRow([Head|RestOfRow]):-
	(
		Head=black,write('@');
		Head=white,write('O');
		Head=empty,write('-')
	),
	printRow(RestOfRow),!.


printBoard([]):-
	writeln('').

printBoard([Row|RestOfBoard]):-
	printRow(Row),
	printBoard(RestOfBoard),!.



findNextCellSameColor(Board,Color,X,Y,XD,YD,XNR,YNR):-
	X+XD>=0,X+XD=<7,XN is X+XD,
	Y+YD>=0,Y+YD=<7,YN is Y+YD,
	opponent(Color,OppositeColor),
	(
		(getCell(Board,XN,YN,OppositeColor),findNextCellSameColor(Board,Color,XN,YN,XD,YD,XNR,YNR));
		(getCell(Board,XN,YN,Color),XNR=XN,YNR=YN)
	).







test():-
	init_board(IBoard),
	setCell(IBoard,3,4,white,Board1),
	setCell(Board1,4,4,white,Board2),
	setCell(Board2,4,5,white,Board3),
	setCell(Board3,4,3,white,Board4),
	setCell(Board4,2,3,white,Board5),
	printBoard(Board5),
	findNextCellSameColor(Board5,white,4,5,-1,-1,XNR,YNR),!,
	writeln(XNR),
	writeln(YNR),
	countBlacks(Board4,Blacks),
	countWhites(Board4,Whites),
	write("BlackCount:"),writeln(Blacks),
	write("WhiteCount:"),writeln(Whites).











	
