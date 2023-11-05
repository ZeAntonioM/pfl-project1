:-ensure_loaded('game_states.pl').

% Play calls the main menu
play:- isaac_menu.

% Main menu
% draws the menu, reads the input and calls the gamemode. If the input is 10, the game ends. 
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

% 1 - Player vs Player
gamemode(1) :- play_game.

% 2 - Player vs Easy IA
gamemode(2) :- play_game.

% 3 - Easy IA vs Player
gamemode(3) :- play_game.

% 4 - Player vs Hard IA
gamemode(4) :- play_game.

% 5 - Hard IA vs Player
gamemode(5) :- play_game.

% 6 - Easy IA vs Hard IA
gamemode(6) :- play_game.

% 7 - Hard IA vs Easy IA
gamemode(7) :- play_game.

% 8 - Easy IA vs Easy IA
gamemode(8) :- play_game.

% 9 - Hard IA vs Hard IA
gamemode(9) :- play_game.

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

% congrats(+Winner) prints the winner
congrats(Winner):-
    nl,
    print_string_(Winner),
    write(' Player Won this Game'),nl,nl.


