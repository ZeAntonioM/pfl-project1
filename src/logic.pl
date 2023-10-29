
% Move in second phase. Checks if it's possible to remove a piece from the board, and if so, removes it. After that, calculates the points and finishes the move.
move2P(Board, Player, Piece, NewBoard, SCB, SCW) :-
    check_move(Board, Player, Piece, SCB, SCW),
    remove_piece(Board, Piece, NewBoard),
    calculate_points(Board, Player, Piece, Points),
    finish_move(Board, Player, Piece, Points, NewBoard).

% Checks if the move is valid
check_move(Board, Player, Piece, SCB, SCW) :-
    get_piece(Board, Piece, PieceType, PiecePositions),
    Player == PieceType, %Checks if the piece is in the player's color
    check_piece(Board, PieceType, PiecePositions, SCB, SCW). %Checks if there is a Score Counter in the piece's positions and if the piece is in the player's color

% removes the piece from the board
remove_piece(Board, Piece, NewBoard) :-
    get_piece(Board, Piece, PieceType, PiecePositions),
    remove_from_board(Board, PiecePositions, NewBoard).

% Calculates the points of the move
calculate_points(Board, Player, Piece, Points) :-
    get_piece(Board, Piece, PieceType, PiecePositions),
    calculate_points_aux(Board, Player, PieceType, PiecePositions, Points).

% Finishes the move
finish_move(Board, Player, Piece, Points, NewBoard) :-
    write('You have removed a piece from the board and can score '), write(Points), write(' points.'), nl,
    write('Choose the number of points you want to score: '), nl,
    read(PointsToScore),
    moveSP(Board, Player, PointsToScore, NewBoard).
    

remove_from_board(Board, [], NewBoard).
remove_from_board(Board, [PiecePosition | PiecePositions], NewBoard) :-
    remove_from_board(Board, PiecePositions, TempBoard),
    remove_from_board_aux(Board, PiecePosition, TempBoard, NewBoard).

