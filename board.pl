


%%%%%%%%%%% INIT THE BOARD %%%%%%%%%%%%%%%%%%%%%%

init_board(Board):-
	Board0 = [
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,white,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty]
	],
	setCell(Board0,3,3,white,Board1),
	setCell(Board1,4,4,white,Board2),
	setCell(Board2,3,4,black,Board3),
	setCell(Board3,4,3,black,Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






getCell(Board,X,Y,Cell):-
	nth0(Y,Board,Row),
	nth0(X,Row,Cell).


setItemInList([_|RestOfList],0,NewValue,[NewValue|RestOfList]).

setItemInList([Head|RestOfList],X,NewValue,[Head|NewRestOfList]):-
	NewX is X-1,
	setItemInList(RestOfList,NewX,NewValue,NewRestOfList),!.


setCell(Board,X,Y,NewValue,NewBoard):-
	nth0(Y,Board,Row),
	setItemInList(Row,X,NewValue,NewRow),
	setItemInList(Board,Y,NewRow,NewBoard).





%%%% COUNTING THE PIECES ON THE BOARD

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% PRINTING THE BOARD %%%%%%%%%%%%%%%%%%

printtopline():- 
	write("\n\n\n  0   1   2   3   4   5   6   7   \n"),
	write("\u2554\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2566\u2550\u2550\u2550\u2557\n").

printRow([]).

printRow([Head|RestOfRow]):-
	write("\u2551 "),
	(
		Head=white,write('\u25C9');
		Head=black,write('\u25CE');
		Head=empty,write('\u0020')
	),
	write(" "),
	printRow(RestOfRow),!.

printnicerow(Row,X):-
	printRow(Row),
	write("\u2551   "),
	writeln(X).

printniceboard(Board):-
	printtopline(),
	printBoard(Board,0).


printBoard([],_):-
	writeln('').

printBoard([Row|RestOfBoard],X):-
	Xs is X+1,
	printnicerow(Row,X),
	(
		(X<7,write("\u2560\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u256C\u2550\u2550\u2550\u2563\n"))
	;
		(X=7,write("\u255A\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u2569\u2550\u2550\u2550\u255D\n"))
	),
	printBoard(RestOfBoard,Xs),!.

printstatus(Board):-
	countBlacks(Board,Blacks),
	countWhites(Board,Whites),
	write("BlackCount:"),writeln(Blacks),
	write("WhiteCount:"),writeln(Whites).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%% HELPER FUNCTIONS FOR BOARD %%%%%%%%%%%%%%%%%%%%%%%

opponent(white,black).
opponent(black,white).

isCellEmpty(Board,X,Y):-
	X<0;X>7;Y<0;Y>7;getCell(Board,X,Y,empty).

areAllNeighboursEmpty(Board,X,Y):-
	XT is X+1,XB is X-1,YT is X+1,YB is Y-1,
	isCellEmpty(Board,XB,YB),
	isCellEmpty(Board,XB,Y),
	isCellEmpty(Board,XB,YT),
	isCellEmpty(Board,X,YB),
	isCellEmpty(Board,X,YT),
	isCellEmpty(Board,XT,YB),
	isCellEmpty(Board,XT,Y),
	isCellEmpty(Board,XT,YT).

hasOpponentColorNeighbour(Board,X,Y,Color):-
	(
		XT is X+1,XB is X-1,YT is X+1,YB is Y-1,opponent(Color,OColor)
	),
	(	
		getCell(Board,XB,YB,OColor),!;
		getCell(Board,XB,Y,OColor),!;
		getCell(Board,XB,YT,OColor),!;
		getCell(Board,X,YB,OColor),!;
		getCell(Board,X,YT,OColor),!;
		getCell(Board,XT,YB,OColor),!;
		getCell(Board,XT,Y,OColor),!;
		getCell(Board,XT,YT,OColor)
	).


findNextCellSameColor(Board,Player,X,Y,XD,YD,XNR,YNR):-
	X+XD>=0,X+XD=<7,XN is X+XD,
	Y+YD>=0,Y+YD=<7,YN is Y+YD,
	opponent(Player,OtherPlayer),
	(
		(getCell(Board,XN,YN,OtherPlayer),findNextCellSameColor(Board,Player,XN,YN,XD,YD,XNR,YNR));
		(getCell(Board,XN,YN,Player),XNR=XN,YNR=YN)
	).

isValidMoveDirection(Board,Player,X,Y,XD,YD,XNR,YNR):-
	findNextCellSameColor(Board,Player,X,Y,XD,YD,XNR,YNR),
	(
		(X+XD+XD >  1);
		(X+XD+XD < -1);
		(Y+YD+YD >  1);
		(Y+YD+YD < -1)
	).


isPotentialValidMove(Board,X,Y,Player):-
	isCellEmpty(Board,X,Y),
	hasOpponentColorNeighbour(Board,X,Y,Player).

isValidMove(Board,X,Y,Player):-
	isPotentialValidMove(Board,X,Y,Player),
	(
		isValidMoveDirection(Board,Player,X,Y, 1, 1,_,_);
		isValidMoveDirection(Board,Player,X,Y, 0, 1,_,_);
		isValidMoveDirection(Board,Player,X,Y,-1, 1,_,_);
		isValidMoveDirection(Board,Player,X,Y, 1, 0,_,_);
		isValidMoveDirection(Board,Player,X,Y,-1, 0,_,_);
		isValidMoveDirection(Board,Player,X,Y, 1,-1,_,_);
		isValidMoveDirection(Board,Player,X,Y, 0,-1,_,_);
		isValidMoveDirection(Board,Player,X,Y,-1,-1,_,_)
	).

colorLine(Board,Color,X,Y,_,_,X,Y,Newboard):-
	setCell(Board,X,Y,Color,Newboard).

colorLine(Board,Color,X,Y,XD,YD,XNR,YNR,Newboard):-
	setCell(Board,X,Y,Color,Boardwithset),
	NewX is X+XD,NewY is Y+YD,
	colorLine(Boardwithset,Color,NewX,NewY,XD,YD,XNR,YNR,Newboard).


move(Board0,X,Y,Player,Board8):-
	(
		(isValidMoveDirection(Board0,Player,X,Y, 1, 1,RUX,RUY);(RUX=X,RUY=Y)), %Right Up
		(isValidMoveDirection(Board0,Player,X,Y, 0, 1,UX ,UY );(UX =X,UY =Y)), %      Up
		(isValidMoveDirection(Board0,Player,X,Y,-1, 1,LUX,LUY);(LUX=X,LUY=Y)), %Left  Up
		(isValidMoveDirection(Board0,Player,X,Y, 1, 0,RX, RY );(RX =X,RY =Y)), %Right
		(isValidMoveDirection(Board0,Player,X,Y,-1, 0,LX, LY );(LX =X,LY =Y)), %Left
		(isValidMoveDirection(Board0,Player,X,Y, 1,-1,RDX,RDY);(RDX=X,RDY=Y)), %Right Down
		(isValidMoveDirection(Board0,Player,X,Y, 0,-1,DX ,DY );(DX =X,DY =Y)), %      Down
		(isValidMoveDirection(Board0,Player,X,Y,-1,-1,LDX,LDY);(LDX=X,LDY=Y))  %Left  Down
	),
	colorLine(Board0,Player,X,Y, 1, 1,RUX,RUY,Board1),
	colorLine(Board1,Player,X,Y, 0, 1,UX ,UY ,Board2),
	colorLine(Board2,Player,X,Y,-1, 1,LUX,LUY,Board3),
	colorLine(Board3,Player,X,Y, 1, 0,RX ,RY ,Board4),
	colorLine(Board4,Player,X,Y,-1, 0,LX, LY, Board5),
	colorLine(Board5,Player,X,Y, 1,-1,RDX,RDY,Board6),
	colorLine(Board6,Player,X,Y, 0,-1,DX, DY ,Board7),
	colorLine(Board7,Player,X,Y,-1,-1,LDX,LDY,Board8).


test():-
	init_board(Board1),
	printniceboard(Board1),
	printstatus(Board1),
	write("\n\nX:"),read(X),
	write("\n\nY:"),read(Y),
	isValidMove(Board1,X,Y,black),move(Board1,X,Y,black,Board2),
	printniceboard(Board2).

humanMove(Board,Player,NextPlayer,NewBoard):-
	write("This is "), write(Player), writeln('s move'),
	writeln("Enter X:"),
	read(X),
	writeln("Enter Y:"),
	read(Y),
	isValidMove(Board,X,Y,Player) -> (move(Board,X,Y,Player,NewBoard),opponent(Player,NextPlayer);(NextPlayer=Player,NewBoard=Board)).


play():-
	init_board(Board0),
	play(Board0,black).


play(Board,Player):-
	printniceboard(Board),
	printstatus(Board),
	humanMove(Board,Player,NextPlayer,NextBoard),
	play(NextBoard,NextPlayer).












	
