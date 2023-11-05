:-ensure_loaded('game_states.pl').

play:- isaac_menu.


isaac_menu :- repeat,
             draw_isaac_menu,
             ask_for_menu_option(Selection),
             read(Selection),
             (
                 Selection = 10;
                 gamemode(Selection),fail
             ).

 %player_robot(Player, easy)
%player_robot(Player,hard)

gamemode(1) :- play_game.

gamemode(2) :- play_game.
gamemode(3) :- play_game.
gamemode(4) :- play_game.
gamemode(5) :- play_game.
gamemode(6) :- play_game.
gamemode(7) :- play_game.
gamemode(8) :- play_game.
gamemode(9) :- play_game.

play_game :-
            retractall(piece_position(_,_,_)),
            retractall(sc(_,_)),
            retractall(bpr(_,_)),
            % Sets the board
            assertz((piece_position(_,_,_):-fail)),
            asserta(sc("W",0)),
            asserta(sc("B",0)),
            asserta(bpr("W",0)),
            asserta(bpr("B",0)),
            %populate,
            game_state(start, "W").
    

game_state(GameState, _):-
       game_over(GameState, Winner),!,
       congrats(Winner).

game_state(GameState, Player):-
    display(GameState, Player),
    ask_for_move(GameState,Player,Move),
    move(GameState,Move, NewGameState),
    change_player(NewGameState,Player, Next_Player),!,
    game_state(NewGameState, Next_Player).


game_state(_,_):-write('Fail').

game_over(GameState,"White"):-
    (GameState= both_players_remove;
     GameState = one_player_remove
    ),
    sc("W",100).

game_over(GameState,"Black"):-
    (GameState= both_players_remove;
     GameState = one_player_remove
    ),
    sc("B",100).

game_over(end_game,"White"):-
    sc("B",SCB),
    sc("W",SCW),
    SCW > SCB.

game_over(end_game,"Black"):-
    sc("B",SCB),
    sc("W",SCW),
    SCW < SCB.
    
game_over(end_game, Player):-
    length_remaining_pieces("B", LenghtB),
    length_remaining_pieces("W", LenghtW),
    biggest_lenght(LenghtB, LenghtW, Player).

game_over(end_game,"Black").



