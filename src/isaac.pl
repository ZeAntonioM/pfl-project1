:-ensure_loaded('io.pl').
:-ensure_loaded('logic.pl').

play:-
    % Removes previous game
    retractall(piece_position(_,_,_)),

    % Sets the board
    asserta((piece_position(_,_,_):-fail.)),

    % Menu e ver o modo 
    isaac_menu(Gamemode),

    % Set o gamemode e Play
    gamemode(Gamemode),

    % Verifies if wants to play again
    playagain(Gamemode).




% Game cycle
playagain:-
    Gamemode \= 5,
    play.

playagain(5).

% 1 - Player vs Player
gamemode(1):-
    phase1cycle("B", FirstPlayerToFinish),
    phase2cycle(FirstPlayerToFinish,0,0,0),
    check_winner(Won).

check_winner(0):-

    %see length of pieces not placed
    
.

check_winner("B").

check_winner("W").

% 2 - Player vs Computer
gamemode(2):-
    
.

% 3 - Computer vs Human
gamemode(3):-
    
.

% 4 - Computer vs Computer
gamemode(4):-
    
.

% 5 - Exit the game
gamemode(5).

%while the 2 players can play
phase1cycle(Player, First_Player_To_Finish):-

    can_place_piece(Player),
    ask_for_piece_to_add(Player),
    change_player(Player, Next_Player),
    phase1cycle(Next_Player, First_Player_To_Finish).


%if first player finishes
phase1cycle(Player, Player):-

    change_player(Player, Next_Player),
    can_place_piece(Next_Player),
    repeat,
    ask_for_piece_to_add(Player),    
    \+ can_place_piece(Next_Player).


phase2cycle(Player, SCB, SCW, Won):-

    (SCB >= 100,
    write('Black won!'),
    Won is "B");
    (SCW >= 100,
    write('White won!'),
    Won is "W").
    

phase2cycle(Player, SCB, SCW, Won):-

    can_remove_piece(Player),
    ask_for_piece_to_remove(Piece_id),
    move2P(Player, Piece_id, SCB, SCW),
    change_player(Player, NewPlayer),
    phase2cycle(NewPlayer, SCB, SCW).
    
phase2cycle(Player, SCB, SCW, Won):-

    change_player(Player, NewPlayer),
    (repeat,
    ask_for_piece_to_remove(Piece_id),
    move2P(Player, Piece_id, SCB, SCW),
    \+ can_remove_piece(NewPlayer)),
    check_winner(Won).
    

