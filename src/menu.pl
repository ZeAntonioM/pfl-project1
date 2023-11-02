% Define a predicate that draws the top and bottom borders of the square
draw_top_bottom_border :-
    write('|-----------------------------------------------------------------------------|'), nl.

draw_left_right_border :-
    write('|                                                                             |'), nl.

draw_isaac :-
    write('|   __________   ________        ____             ____            ________    |'), nl,
    write('|  |          | |        |      /    \\           /    \\         /         |   |'), nl,
    write('|   ---|  |---  |    ----      |  /\\  |         |  /\\  |       /          |   |'), nl,
    write('|      |  |     |   \\         /  /  \\  \\       /  /  \\  \\      |    ------    |'), nl,
    write('|      |  |      \\   --\\     |  /----\\  |     |  /----\\  |     |    |         |'), nl,
    write('|      |  |       \\--   \\   /           \\    /           \\     |    ______    |'), nl,
    write('|   ---|  |---  ___ /    | |  /-------\\  |  |  /-------\\  |    \\          |   |'), nl,
    write('|  | ________ ||________/ /__/         \\__\\/__/         \\__\\    \\---------    |'), nl,
    write('|                                                                             |'), nl.


% Define a predicate that draws the game options inside the square
draw_game_options :-
    draw_left_right_border,
    draw_left_right_border,
    write('|                              1. Player vs Player                            |'), nl,
    draw_left_right_border,
    draw_left_right_border,
    write('|                              2. Player vs IA                                |'), nl,
    draw_left_right_border,
    draw_left_right_border,
    write('|                              3. IA vs IA                                    |'), nl,
    draw_left_right_border,
    draw_left_right_border,
    write('|                              4. Exit                                        |'), nl,
    draw_left_right_border,
    draw_left_right_border.

% Modify the ISAAC menu
isaac_menu :-
    % Draw the square
    draw_top_bottom_border,
    draw_isaac,
    draw_game_options,
    draw_top_bottom_border,
    % Get the user's choice
    read(Choice),
    % Handle the user's choice
    (Choice = 1 -> player_vs_player;
     Choice = 2 -> player_vs_ia;
     Choice = 3 -> ia_vs_ia;
     Choice = 4 -> true;
     isaac_menu).
