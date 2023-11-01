# Isaac Board Game - PFL First Project

## Description

This is an implementation in Prolog of the board game "Isaac", which was created at the end of 2010. The game consists of two phases: the placement phase and the scoring phase. The goal is to score at least 100 points during the scoring phase or have the highest score at the end of the game.

## ISAAC - Rules

### Board

Isaac uses a 10x10 board, with different numerical values for each player. Each player will see numbers from 1 to 10 on his bottom side of the board, and numbers from 00 to 90 on his left side of the board. The board is initially empty.
Each player has 15 pieces of 5 different sizes, each one having is own value: 
- 5 pieces of size 3, value 1;
- 4 pieces of size 4, value 2;
- 3 pieces of size 5, value 3;
- 2 pieces of size 6, value 4;
- 1 piece of size 7, value 6. 

### Placement Phase

The game always starts with the placement phase. Starting with the Dark player, players take turns placing one of their pieces on empty cells of the board. If a player cannot place a valid piece, he passes, and the other player plays until bot players have no more valid moves.
The placement phase ends when both players have no more space to place pieces on the board.
The players who have not placed all their pieces will align their remaining pieces in order to form a line of pieces. The length of the line will determine the player who wins if the players tie in the scoring phase.
Now, the player who passed first in the placement phase starts the scoring phase.

### Scoring phase

#### Removing Piece

Both players start with 0 points. Players take turns removing one piece from the board, starting with the player who passed first in the placement phase. The removed piece must be at least as large as the largest piece removed previously by the same player. A player cannot remove a piece lying under the score counter(of any colors).

#### Scoring

This game has a specific method to score. Both players start with 0 points, and the score is calculated as follows:

- Count the number of pieces left in the line of the removed piece. This is the base score.
- Then, we multiply this number by the value of the removed piece. This is the multiplied score.
- For each counter lying on the line, we multiply the final score by 2. This is the final score.

#### Moving the Score Counter

After scoring, the player may move his score counter a number of cells between 0 and the final score. The score counter cannot be placed on a cell that already contains a score counter.

When a player scores more than 100 points, he wins the game. If both players pass, the player with the highest score wins.

## Run the game

- Open Sicstus Prolog
- run `consult('isaac.pl').`

## Authors

### Team Isaac_3:

- [Francisco Cardoso](https://github.com/FranciscoCardoso913), up202108793
- [José Martins](https://github.com/ZeAntonioM), up202108794
