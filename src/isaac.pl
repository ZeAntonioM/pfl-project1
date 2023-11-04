:-ensure_loaded('io.pl').
:-ensure_loaded('logic.pl').

play:-
    % Removes previous game
    retractall(piece_position(_,_,_)),

    % Sets the board
    asserta((piece_position(_,_,_):-fail)),

    populate,

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
    phase(players_add_pieces,"B",First_player_to_finish),
    phase(both_player_remove_pieces,First_player_to_finish, Scb,Scw, BRP, Next_Player ),
    (
        (
            (Scb>=100;
            Scw>=100),
            winner(Scb,Scw),!
            );
        (
            phase(one_player_removes_pieces,Next_Player,BRP, Scb, Scw,New_Scb, New_Scw),
            winner(New_Scb,New_Scw),!
        )
     ),
    
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
phase(players_add_pieces,Player, First_Player_To_Finish):-

    can_place_piece(Player, _, _, _),
    ask_for_piece_to_add(Player),
    change_player(Player, Next_Player),
    phase(players_add_pieces,Next_Player, First_Player_To_Finish).


%if first player finishes
phase(players_add_pieces,Player, Player):-
    change_player(Player, Next_Player),
    can_place_piece(Next_Player, _, _, _),
    repeat,
    ask_for_piece_to_add(Player),    
    \+ can_place_piece(Next_Player, _, _, _).

phase(players_add_pieces,Player, Player):-
    write('Both players cannot play anymore!'),
    draw_board(Player).
    

phase(both_player_remove_pieces,Player, Scb,Scw, BRP, First_Player_To_Stop ):-
    phase(both_player_remove_pieces,Player, 0,0,0,0, Scb,Scw, BRP, First_Player_To_Stop)
    .
 
phase(Phase,_, Scb,Scw,_,_, Scb,Scw, _, _):-
    (Phase =both_player_remove_pieces;
     Phase = one_player_removes_pieces ),
    (Scb>=100;
    Scw>=100).
 
phase(both_player_remove_pieces,Player, Scb,Scw,BRPB,BRPW, F_Scb,F_Scw, BRP, F_Player):-

    can_remove_pieces(Player, BRPB, BRPW, Scb, Scw),
    ask_for_piece_to_remove(Player, Piece, Direction, Position, BRPB, BRPW, Scb, Scw),!,
    remove_piece(Piece),
    update_biggest_piece(Player,Piece,BRPB, BRPW, New_BRPB, New_BRPW),
    calculate_points( Piece, Position, Direction, Scb, Scw, Points),
    get_points_to_score(Points, PointsToScore),
    score_points(Player, Scb, Scw, PointsToScore, NewSCB, NewSCW),
    change_player(Player, Next_P),
    phase(both_player_remove_pieces,Next_P, NewSCB,NewSCW,New_BRPB,New_BRPW, F_Scb,F_Scw, BRP, F_Player)
    . 

phase(both_player_remove_pieces,"B", Scb,Scw,_BRPB,BRPW, Scb,Scw, BRPW, "W").
phase(both_player_remove_pieces,"W", Scb,Scw,BRPB,_BRPW, Scb,Scw, BRPB, "B").

phase(one_player_removes_pieces,Player ,BRP, Scb, Scw,F_Scb, F_Scw):-
    can_remove_pieces(Player, BRP, BRP, Scb, Scw),
    ask_for_piece_to_remove(Player, Piece, Direction, Position, BRP, BRP, Scb, Scw),
    remove_piece(Piece),
    update_biggest_piece(Piece, BRP,NEW_BRP),
    calculate_points( Piece, Position, Direction, Scb, Scw, Points),
    get_points_to_score(Points, PointsToScore),
    score_points(Player, Scb, Scw, PointsToScore, NewSCB, NewSCW),
    phase(one_player_removes_pieces,Player, NEW_BRP,NewSCB,NewSCW, F_Scb,F_Scw).

phase(one_player_removes_pieces,_Player ,_BRP, Scb, Scw,Scb, Scw).

winner(Scb, Scw):-
    Scb>Scw,nl,
    write('Black Won'),nl
    .

winner(Scb, Scw):-
    Scw>Scb,
    nl,
    write('White Won'),
    nl
    .
winner(_,_):-nl,write('Hallo'),nl.
