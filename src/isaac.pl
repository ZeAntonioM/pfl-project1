:-ensure_loaded('game_states.pl').

play:- isaac_menu.

isaac_menu :-
            retractall(player_robot(_,_)),
            assertz((player_robot(_,_):-fail)),
            draw_isaac_menu,
            ask_for_menu_option(Selection),
            (
                Selection = 10;
                gamemode(Selection)
            ).

gamemode(1) :- play_game.

gamemode(2) :- 
    init_random_state,
    asserta(player_robot("W", easy)),
    play_game.

gamemode(3) :- 
    init_random_state,
    asserta(player_robot("B", easy)),
    play_game.

gamemode(4) :-
    init_random_state,
    asserta(player_robot("W", hard)),
    play_game.

gamemode(5) :-
    init_random_state,
    asserta(player_robot("B", hard)),
    play_game.

gamemode(6) :- 
    init_random_state,
    asserta(player_robot("B", easy)),
    asserta(player_robot("W", hard)),
    play_game.

gamemode(7) :-
    init_random_state,
    asserta(player_robot("B", hard)),
    asserta(player_robot("W", easy)),
    play_game.

gamemode(8) :- 
    init_random_state,
    asserta(player_robot("B", easy)),
    asserta(player_robot("W", easy)),
    play_game.

gamemode(9) :- 
    init_random_state,
    asserta(player_robot("B", hard)),
    asserta(player_robot("W", hard)),
    play_game.

gamemode(10).

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

congrats(Winner):-
    nl,
    print_string_(Winner),
    write(' Player Won this Game'),nl,nl,
    isaac_menu.


