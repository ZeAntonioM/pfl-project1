:-ensure_loaded('draw.pl').

isaac_menu:-
    draw_isaac_menu,
    % Get the user's choice
    write('Choose an option(remember to always put a . at the end of the number): ')
    read(Choice),
    % Handle the user's choice
    (Choice = 1 -> player_vs_player;
     Choice = 2 -> player_vs_ia;
     Choice = 3 -> ia_vs_ia;
     Choice = 4 -> true;
     isaac_menu).
