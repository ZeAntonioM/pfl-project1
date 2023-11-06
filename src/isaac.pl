:-ensure_loaded('game_states.pl').

% Play calls the main menu
play:- isaac_menu.


% Isaac_menu is the main menu of the game
% draws the menu, reads the input and calls the gamemode. If the input is 10, the game ends. 
isaac_menu :-
            retractall(player_robot(_,_)),
            assertz((player_robot(_,_):-fail)),
            draw_isaac_menu,
            ask_for_menu_option(Selection),
            (
                Selection = 10;
                gamemode(Selection)
            ).

%%%%%%%%%%%%%% gamemode %%%%%%%%%%%%%%
% gamemode is the predicate that calls the game mode selected by the user
% 1 - Player vs Player
gamemode(1) :- play_game.

% 2 - Player vs Easy IA
gamemode(2) :- 
    init_random_state,
    asserta(player_robot("W", easy)),
    play_game.

% 3 - Easy IA vs Player
gamemode(3) :- 
    init_random_state,
    asserta(player_robot("B", easy)),
    play_game.

% 4 - Player vs Hard IA
gamemode(4) :-
    init_random_state,
    asserta(player_robot("W", hard)),
    play_game.

% 5 - Hard IA vs Player
gamemode(5) :-
    init_random_state,
    asserta(player_robot("B", hard)),
    play_game.

% 6 - Easy IA vs Hard IA
gamemode(6) :- 
    init_random_state,
    asserta(player_robot("B", easy)),
    asserta(player_robot("W", hard)),
    play_game.

% 7 - Hard IA vs Easy IA
gamemode(7) :-
    init_random_state,
    asserta(player_robot("B", hard)),
    asserta(player_robot("W", easy)),
    play_game.

% 8 - Easy IA vs Easy IA
gamemode(8) :- 
    init_random_state,
    asserta(player_robot("B", easy)),
    asserta(player_robot("W", easy)),
    play_game.

% 9 - Hard IA vs Hard IA
gamemode(9) :- 
    init_random_state,
    asserta(player_robot("B", hard)),
    asserta(player_robot("W", hard)),
    play_game.


%%%%%%%%%%%%%% Play Game %%%%%%%%%%%%%%
% play_game prepares the game and starts it
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
    

%%%%%%%%%%%%%% Game State %%%%%%%%%%%%%%
% game_state(+GameState, +Player) is the main game loop
% if the game is over, it prints the winner
game_state(GameState, _):-
       game_over(GameState, Winner),!,
       congrats(Winner).

% if the game is not over, it displays the board, asks for a move, makes the move and changes the player
game_state(GameState, Player):-
    display(GameState, Player),
    ask_for_move(GameState,Player,Move),
    move(GameState,Move, NewGameState),
    change_player(NewGameState,Player, Next_Player),!,
    game_state(NewGameState, Next_Player).

% debug questions
game_state(_,_):-write('Fail').


%%%%%%%%% Game Over %%%%%%%%%%%%%
% game_over(+GameState, -Winner) checks if the game is over and returns the winner
% If the game is on the 2nd phase and the Score Counter of the White Player is 100, then the winner is the White Player
game_over(GameState,"White"):-
    (GameState= both_players_remove;
     GameState = one_player_remove
    ),
    sc("W",100).

% If the game is on the 2nd phase and the Score Counter of the Black Player is 100, then the winner is the Black Player
game_over(GameState,"Black"):-
    (GameState= both_players_remove;
     GameState = one_player_remove
    ),
    sc("B",100).

% If the second phase ended, if the White Player has more points than the Black Player, then the winner is the White Player
game_over(end_game,"White"):-
    sc("B",SCB),
    sc("W",SCW),
    SCW > SCB.

% If the second phase ended, if the Black Player has more points than the White Player, then the winner is the Black Player
game_over(end_game,"Black"):-
    sc("B",SCB),
    sc("W",SCW),
    SCW < SCB.

% If the second phase ended, if the Black Player and the White Player have the same amount of points, then the winner is the 
% player with the biggest line of pieces remaining
game_over(end_game, Player):-
    length_remaining_pieces("B", LenghtB),
    length_remaining_pieces("W", LenghtW),
    biggest_lenght(LenghtB, LenghtW, Player).

% else, the winner is the player who started the game.
game_over(end_game,"Black").


