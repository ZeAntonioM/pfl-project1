:-ensure_loaded('io.pl').
:-ensure_loaded('logic.pl').

play:-
    % Removes previous game
    retractall(piece_position(_,_,_)),

    % Sets the board
    asserta((piece_position(_,_,_):-fail)),

    %populate(10,99),

    % Menu e ver o modo 
    isaac_menu(Gamemode),

    % Set o gamemode e Play
    gamemode(Gamemode),

    % Verifies if wants to play again
    playagain(Gamemode).


% Game cycle
playagain(Gamemode):-
    Gamemode \= 5,
    play.

playagain(5).


check_winner(0).%:-

    %see length of pieces not placed
    
%.    


check_winner("B").

check_winner("W").

% 1 - Player vs Player
gamemode(1):-
    phase1cycle("B", FirstPlayerToFinish),
    phase2cycle(FirstPlayerToFinish, 0, 0, 0, 0, 0).
    %check_winner(Won).

% 2 - Player vs Computer
gamemode(2).%:-
    
%.

% 3 - Computer vs Human
gamemode(3).%:-
    
%.

% 4 - Computer vs Computer
gamemode(4).%:-
    
%.

% 5 - Exit the game
gamemode(5).

%while the 2 players can play
phase1cycle(Player, First_Player_To_Finish):-

    can_place_piece(Player, _, _, _),
    ask_for_piece_to_add(Player),
    change_player(Player, Next_Player),
    phase1cycle(Next_Player, First_Player_To_Finish).


%if first player finishes
phase1cycle(Player, Player):-

    change_player(Player, Next_Player),
    can_place_piece(Next_Player, _, _, _),
    repeat,
    ask_for_piece_to_add(Player),    
    \+ can_place_piece(Next_Player, _, _, _).

phase1cycle(Player,Player):-
    write('Both players cannot play anymore!'),
    draw_board(Player).
    

%phase2cycle(Player, SCB, SCW, BiggestPieceB, BiggestPieceW, Won):-
%
%    can_remove_pieces(Player, BiggestPieceB, BiggestPieceW, SCB, SCW),
%    ask_for_piece_to_remove(Player, Piece, Positions),
%    remove_piece(Piece),
%    calculate_points(Player, Piece, Direction, Positions, SCB, SCW, Points),
%    finish_move(Player, SCB, SCW, Piece, Points).
%    change_player(Player, NewPlayer),
%    phase2cycle(NewPlayer, SCB, SCW, BiggestPieceB, BiggestPieceW, Won).
%    
%phase2cycle(Player, SCB, SCW, BiggestPieceB, BiggestPieceW, Won):-
%
%    change_player(Player, NewPlayer),
%    (repeat,
%    ask_for_piece_to_remove(Player, Piece),
%    remove_piece(Piece),
%    calculate_points(Player, Piece, Direction, Positions, SCB, SCW, Points),
%    finish_move(Player, SCB, SCW, Piece, Points).
%    \+ can_remove_piece(NewPlayer, BiggestPieceB, BiggestPieceW, SCB, SCW),
%    check_winner(Won).
%    
%phase2cycle(Player, SCB, SCW, BiggestPieceB, BiggestPieceW, Won).

%normal cycle of phase 2
phase2cycle(Player, SCB, SCW, BiggestPieceB, BiggestPieceW):-

    can_remove_pieces(Player, BiggestPieceB, BiggestPieceW, SCB, SCW),
    ask_for_piece_to_remove(Player, Piece, Direction, Position, BiggestPieceB, BiggestPieceW, SCB, SCW),
    remove_piece(Piece),
    calculate_points(Player, Piece, Position, Direction, SCB, SCW, Points),
    get_points_to_score(Points, PointsToScore),
    score_points(Player, SCB, SCW, PointsToScore, NewSCB, NewSCW),
    loop2phase(Player, NewSCB, NewSCW, BiggestPieceB, BiggestPieceW).

% One player cannot remove pieces
phase2cycle(Player, SCB, SCW, BiggestPieceB, BiggestPieceW):-

    can_remove_pieces(Player, BiggestPieceB, BiggestPieceW, SCB, SCW),
    repeat,
    ask_for_piece_to_remove(Player, Piece, Direction, Position, BiggestPieceB, BiggestPieceW, SCB, SCW),
    remove_piece(Piece),
    calculate_points(Player, Piece, Position, Direction, SCB, SCW, Points),
    get_points_to_score(Points, PointsToScore),
    score_points(Player, SCB, SCW, PointsToScore, NewSCB, NewSCW),
    (check_continue_2_phase(Player, NewSCB, NewSCW);
    /+ can_remove_pieces(Player, BiggestPieceB, BiggestPieceW, SCB, SCW)).

%desempate