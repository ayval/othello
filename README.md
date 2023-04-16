# othello
Othello project in prolog
Programmers: Avyal Ron And Dor Zohar
Description taken from wikipedia (https://en.wikipedia.org/wiki/Reversi).
Description: Othello is a strategy board game for two players, played on an 8×8 uncheckered board.
There are sixty-four identical game pieces called disks (often spelled "discs"),
which are light on one side and dark on the other.
Players take turns placing disks on the board with their assigned color facing up.
During a play, any disks of the opponent's color that are in a straight line
and bounded by the disk just placed and another disk
of the current player's color are turned over to the current player's color.
Input: play(x). where x is between 1 to 5 (for example play(3). ). (the number is represent the deep that alphabeta search will do,
so by that its mean the diffuclty of the game).
For advanced games input play(X,N). where N is the board size (NXN). 
Output: Win/Lose after playing.
Synopsys: the @ represent the white player (Computer) and # is you, the player.
in each turn the player need to submit the X cordinate, and afterwards (the output direct if the X was valid)
the player has to submit the Y cordinate of the coin that he want to put on the board.
