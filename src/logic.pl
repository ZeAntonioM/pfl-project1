:-ensure_loaded('pieces.pl').

%game_state(Board, Player, Phase, SCB, SCW, Win):-
%   Win = 0, % 0 - Game is not over, 1 - Game is over
%.

% Move in second phase. Checks if it's possible to remove a piece from the board, and if so, removes it. After that, calculates the points and finishes the move.
move2P(Board, Player, Piece-PieceType, NewBoard, SCB, SCW) :-
    check_move(Board, Player, Piece, SCB, SCW),
    remove_piece(Board, Piece, NewBoard),
    calculate_points(Board, Player, Piece, Points),
    finish_move(Board, Player, Piece, Points, NewBoard).

% Checks if the move is valid
check_move(Board, Player, Piece-PieceType, SCB, SCW) :-
    Player == PieceType, %Checks if the piece is in the player's color
    check_piece_positions(Board, Piece, SCB, SCW) %Checks if there is a Score Counter in the piece's positions

% removes the piece from the board
remove_piece(Board, Piece, NewBoard) :-
    get_piece_positions(Board, Piece, PiecePositions),
    remove_from_board(Board, PiecePositions, NewBoard).
    Board = NewBoard.

% Calculates the points of the move
calculate_points(Board, Player, Piece, Points) :-
    piece_value(Piece, PointsPerPiece),
    calculate_points_aux(Board, Player, PieceType, PiecePositions, Points).

% Finishes the move
finish_move(Board, Player, Piece, Points, NewBoard) :-
    write('You have removed a piece from the board and can score '), write(Points), write(' points.'), nl,
    write('Choose the number of points you want to score: '), nl,
    read(PointsToScore),
    moveSP(Board, Player, PointsToScore, NewBoard),
    change_player(Player, Player).
    

remove_from_board(Board, [], NewBoard)
remove_from_board(Board, [PiecePosition | R], NewBoard) :-
    remove_from_board(Board, R, TempBoard),
    %remove the piece from the position in Temp Board
.

change_player(Player, NewPlayer) :-
    Player == 'W',
    NewPlayer = 'B'.

change_player(Player, NewPlayer) :-
    Player == 'B',
    NewPlayer = 'W'.


