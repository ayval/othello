%%%%%%%%%%% INIT THE BOARD %%%%%%%%%%%%%%%%%%%%%%

init_board(Board):-
	Board0 = [
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty],
		[empty,empty,empty,empty,empty,empty,empty,empty]
	],
	setCell(Board0,3,3,white,Board1),
	setCell(Board1,4,4,white,Board2),
	setCell(Board2,3,4,black,Board3),
	setCell(Board3,4,3,black,Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% GAME OVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%If game is over, IsOver will be populate with the winning player color
%Winner will be populate 'draw' in case of draw..

game_over(Board,Winner):-
	FullBoard is (8*8), %board size
	countBlacks(Board,Blacks),
	countWhites(Board,Whites),
	(
	Blacks == 0,!,
	Winner = 'white'
	;
	Whites == 0,!,
	Winner = 'black'
	;
		(
		Blacks + Whites =:= FullBoard,!
		;
		getAllValidMoves(Board,black,ValidMoves1),
		getAllValidMoves(Board,white,ValidMoves2),
		ValidMoves1 == [], ValidMoves2 == [],!
		),
		(Blacks > Whites,
		Winner = 'black'
		;
		Whites > Blacks,
		Winner = 'white'
		;
		Winner = 'draw'
		)
	).
	
result(Winner):-
	Winner == 'white',!,
	write("You Lost!! :( ")
	;
	Winner == 'black',!,
	write("You Won!! :) ")
	;
	write("Draw!! ").
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getCell(Board,X,Y,Cell):-
	isValidCell(X,Y),
	nth0(Y,Board,Row),
	nth0(X,Row,Cell),!.
	%;
	%Cell=empty.

isValidCell(X,Y):-
	X>=0,X=<7,Y>=0,Y=<7.


setItemInList([_|RestOfList],0,NewValue,[NewValue|RestOfList]).

setItemInList([Head|RestOfList],X,NewValue,[Head|NewRestOfList]):-
	NewX is X-1,
	setItemInList(RestOfList,NewX,NewValue,NewRestOfList),!.


setCell(Board,X,Y,NewValue,NewBoard):-
	nth0(Y,Board,Row),
	setItemInList(Row,X,NewValue,NewRow),
	setItemInList(Board,Y,NewRow,NewBoard).

%%%%%%%%%%%%%%%%%%%% ALPHA BETA %%%%%%%%%%%%%%%%%%%%%%%%%%

alphabeta(Depth, p(Board,Player), Alpha, Beta, BestMove, Value):-
  Depth > 0,
  getAllValidMoves(Board,Player,Moves),
  Moves = [_|_], !,
  NewDepth is Depth - 1,
  Alpha1 is -Beta,
  Beta1 is -Alpha,
  bestmove(Moves, p(Board,Player), NewDepth, Alpha1, Beta1, 0, BestMove, Value).
  
alphabeta(_, Position, _, _, 0, Value):-
  value(Position, Value). % Depth is 0, or no moves left

bestmove([c(X,Y)|Moves], p(Board,Player), Depth, Alpha, Beta, Move0, Move1, Value1):-
  move(Board, X,Y,Player, NewBoard0), !,
  %swap_position(NewBoard0, NewBoard),
  opponent(Player,NextPlayer),
  alphabeta(Depth, p(NewBoard0,NextPlayer), Alpha, Beta, _, MinusValue),
  Value is -MinusValue,
  cutoff(c(X,Y), Value, Depth, Alpha, Beta, Moves, p(Board,Player), Move0, Move1, Value1).

bestmove([], _, _, Alpha, _, Move, Move, Alpha).

bestmove([], _, _, _, Beta, Move, Move, Beta).

cutoff(_, Value, Depth, Alpha, Beta, Moves, Position, Move0, Move1, Value1):-
  Value =< Alpha, !,
  bestmove(Moves, Position, Depth, Alpha, Beta, Move0, Move1, Value1).
cutoff(Move, Value, Depth, _, Beta, Moves, Position, _, Move1, Value1):-
  Value < Beta, !,
  bestmove(Moves, Position, Depth, Value, Beta, Move, Move1, Value1).
cutoff(Move, Value, _, _, _, _, _, _, Move, Value).

value(p(Board,Player), -100):-
	opponent(Player,NextPlayer),
	game_over(Board,Winner),
	(
		Winner=='white', NextPlayer==white,!
		;
		Winner=='black', NextPlayer==black,!
	).
  

value(p(Board,Player), Value):-
	coinValue(p(Board,Player),CoinsVal),
	cornersValue(p(Board,Player),CornersVal),
	mobilityValue(p(Board,Player),MobilityVal),
	Value is CoinsVal + CornersVal + MobilityVal.
	
	
%%% COVER CoinValue Heuristic Function For Coins Parity of Board %%%
coinValue(p(Board,Player),Value):-
	countBlacks(Board,Blacks),
	countWhites(Board,Whites),
	(
	Player == white,!,
	Value is 100*(Whites-Blacks)/(Whites+Blacks)
	;
	Value is 100*(Blacks-Whites)/(Blacks+Whites),!
	).

%%% COVER CornersValue Heuristic Function For Corners of Board %%%
cornersValue(p(Board,Player),Value):-
	countWhiteCorners(p(Board,Player),WhiteCorners),
	countBlackCorners(p(Board,Player),BlackCorners),
	(
	WhiteCorners + BlackCorners =:= 0,!,
	Value = 0
	;
	Player == white,!,
	Value is 100*(WhiteCorners-BlackCorners)/(WhiteCorners+BlackCorners)
	;
	Value is 100*(BlackCorners-WhiteCorners)/(BlackCorners+WhiteCorners),!
	).

mobilityValue(p(Board,Player),Value):-
	getAllValidMoves(Board,black,BlackMoves),
	getAllValidMoves(Board,white,WhiteMoves),
	length(BlackMoves,BlackCount),
	length(WhiteMoves,WhiteCount),
	%%% COVER MobilityValue Heuristic Function For Actual mobility of Board %%%
	(
		BlackCount + WhiteCount =:= 0,!,
		Value = 0
		;
		Player == white,!,
		Value is 100*(WhiteCount-BlackCount)/(BlackCount + WhiteCount)
		;
		Value is 100*(BlackCount-WhiteCount)/(BlackCount + WhiteCount),!
	).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                        Heuristics                       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% COUNTING THE CORENERS %%%%%

countCorners(Position,Value):-
	countCorners(Position,[(0,0),(0,7),(7,0),(7,7)],Value).

countCorners(_,[],0):-!.

countCorners(p(Board,Player),[(X,Y)|Rest],Value):-
	countCorners(p(Board,Player),Rest,NextValue),
	(
	getCell(Board,X,Y,Player),
	Value is NextValue+1
	;
	Value is NextValue
	).

countBlackCorners(Board,BlackCorners):-
	countCorners(p(Board,black),BlackCorners).

countWhiteCorners(Board,WhiteCorners):-
	countCorners(p(Board,white),WhiteCorners).


%%%% COUNTING THE PIECES ON THE BOARD %%%%%

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
		Head=white,write('w');
		Head=black,write('b');
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


printPossibleMoves([]):-
	writeln("").

printPossibleMoves([c(X,Y)|Rest]):-
	write("("),write(X),write(","),write(Y),write(")"),
	printPossibleMoves(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%% HELPER FUNCTIONS FOR BOARD %%%%%%%%%%%%%%%%%%%%%%%

opponent(white,black).
opponent(black,white).

swap_position([],[]):-!.
swap_position([Row|RestBoard], [NewRow|NewRest]):-
	swap_row(Row,NewRow),
	swap_position(RestBoard,NewRest).

swap_row([],[]):-!.
swap_row([Cell|ResRow],[NewCell|NewRestRow]):-
	(Cell == black,!,
	NewCell = white
	;
	Cell == white,!,
	NewCell = black
	;
	NewCell = empty,!
	),
	swap_row(ResRow,NewRestRow).
	

isCellEmpty(Board,X,Y):-
	getCell(Board,X,Y,empty).

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
	writeln("Enter has oppponent color neighbour"),
	(
		XT is X+1,XB is X-1,YT is X+1,YB is Y-1,opponent(Color,OColor)
	),
	(	
		(writeln(bla0),X>0,Y>0,getCell(Board,XB,YB,OColor));
		(writeln(bla1),X>0,    getCell(Board,XB,Y,OColor));
		(writeln(bla2),X>0,Y<7,getCell(Board,XB,YT,OColor));
		(writeln(bla3),Y>0,    getCell(Board,X,YB,OColor));
		(writeln(bla4),Y<7,    getCell(Board,X,YT,OColor));
		(writeln(bla5),Y>0,X<7,getCell(Board,XT,YB,OColor));
		(writeln(bla6),X<7,    getCell(Board,XT,Y,OColor));
		(writeln(bla7),X<7,Y>7,getCell(Board,XT,YT,OColor))
	),!,
	writeln("\nhasOpponentColorNeighbour returned successfully\n").


findNextCellSameColor(Board,Player,X,Y,XD,YD,XNR,YNR):-
	X+XD>=0,X+XD=<7,XN is X+XD,
	Y+YD>=0,Y+YD=<7,YN is Y+YD,
	opponent(Player,OtherPlayer),
	(
		(getCell(Board,XN,YN,OtherPlayer),findNextCellSameColor(Board,Player,XN,YN,XD,YD,XNR,YNR));
		(getCell(Board,XN,YN,Player),XNR=XN,YNR=YN)
	).

isValidMoveDirection(Board,Player,X,Y,XD,YD,XNR,YNR):-
	findNextCellSameColor(Board,Player,X,Y,XD,YD,XNR,YNR),!,
	(
		(XNR - X >  1);
		(XNR - X < -1);
		(YNR - Y >  1);
		(YNR - Y < -1)
	).


isValidMove(Board,X,Y,Player):-
	isValidCell(X,Y),!,
	isCellEmpty(Board,X,Y),!,
%	hasOpponentColorNeighbour(Board,X,Y,Player),
	(
		isValidMoveDirection(Board,Player,X,Y, 1, 1,_,_),!;
		isValidMoveDirection(Board,Player,X,Y, 0, 1,_,_),!;
		isValidMoveDirection(Board,Player,X,Y,-1, 1,_,_),!;
		isValidMoveDirection(Board,Player,X,Y, 1, 0,_,_),!;
		isValidMoveDirection(Board,Player,X,Y,-1, 0,_,_),!;
		isValidMoveDirection(Board,Player,X,Y, 1,-1,_,_),!;
		isValidMoveDirection(Board,Player,X,Y, 0,-1,_,_),!;
		isValidMoveDirection(Board,Player,X,Y,-1,-1,_,_)
	),!.

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


getInput(X,Msg):-
	writeln(Msg),
	read(X),
	integer(X),!.

getInput(X,Msg):-
	writeln("Invalid Input"),
	getInput(X,Msg).

getAllValidMoves(Board,Player,ValidMoves):-
	findall(c(X,Y),(between(0,7,X),between(0,7,Y)),MoveMatrix),
	getAllValidMoves(Board,Player,MoveMatrix,ValidMoves).

getAllValidMoves(_,_,[],[]).

getAllValidMoves(Board,Player,[c(X,Y)|MoveList],[c(X,Y)|ValidMoves]):-
	isValidMove(Board,X,Y,Player),!,
	getAllValidMoves(Board,Player,MoveList,ValidMoves).


getAllValidMoves(Board,Player,[c(X,Y)|MoveList],ValidMoves):-
	getAllValidMoves(Board,Player,MoveList,ValidMoves).

choose_move(_,Board,Player,Board):-
	getAllValidMoves(Board,Player,ValidMoves),
	ValidMoves == [],!,
	write("No Valid Moves. Turn Pass").
	
choose_move(Depth,Board,white,NewBoard):-
	alphabeta(Depth,p(Board,white), -1000, 1000, c(X,Y), _),
	move(Board,X,Y,white,NewBoard),!.

choose_move(_,Board,black,NewBoard):-
	getAllValidMoves(Board,black,ValidMoves),
	write("Hint, possible moves are:"),
	printPossibleMoves(ValidMoves),
	getInput(X,"Enter X:"),
	getInput(Y,"Enter Y:"),
	isValidMove(Board,X,Y,black),!,
	move(Board,X,Y,black,NewBoard).

choose_move(_,Board,black,NewBoard):-
		writeln("#########################\nIllegal move. Please play again.\n#########################\n"),
		choose_move(_,Board,black,NewBoard).

depth_limits(1, 6).

%%%%%%%%%%%%%%%%%%%%%%% START HERE %%%%%%%%%%%%%%%%%%%%%%%%%%%%
play(Depth):-
	depth_limits(MinDepth, MaxDepth),
	Depth >= MinDepth,
	Depth =< MaxDepth,
	init_board(Board0),
	play(Depth, Board0,black),!
	;
	nl, write(`***Depth is outside limits***`), nl, fail.

play(_, Board, _):-
	game_over(Board,Winner),!,
	result(Winner).
	
play(Depth, Board, Player):-
	printniceboard(Board),
	printstatus(Board),
	choose_move(Depth,Board,Player,NextBoard),
	opponent(Player,NextPlayer),
	play(Depth, NextBoard,NextPlayer).