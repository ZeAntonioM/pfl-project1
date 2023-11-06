:-ensure_loaded('utils.pl').

%%%%%%%%%% Number of pieces per player %%%%%%%%%%%%%%%%
% number_of_Pieces_per_Player(N) - Number of pieces per player. Can be easily changed to allow more pieces per player
number_of_Pieces_per_Player(15).


%%%%%%%%%% Piece %%%%%%%%%%%%%%%%
% Piece(id) - Check if the Piece exists. the id is a number between 1 and 2*number_of_Pieces_per_Player
piece(Id):-
    number_of_Pieces_per_Player(N),
    Id>0,
    Id=<N*2.

%%%%%%%%%% Number of Pieces per size %%%%%%%%%%%%%%%%
% Number_of_Pieces(Piece_Size, Number_of_existing_pieces) - The number of existing pieces (per player) of each size. Can be easily changed to allow more pieces per player.
number_of_Pieces(3,5).
number_of_Pieces(4,4).
number_of_Pieces(5,3).
number_of_Pieces(6,2).
number_of_Pieces(7,1).
number_of_Pieces(_,0).


%%%%%%%%%% Relation Size - Value %%%%%%%%%%%%%%%%
% size_value(Size,Value) - Value corresponding to the size of the piece. Can be easily changed to change the value of a piece.
size_value(3,1).
size_value(4,2).
size_value(5,3).   
size_value(6,4).                                    
size_value(7,6).        

%%%%%%%%%% Piece Owner %%%%%%%%%%%%%%%%
% piece_owner(Piece,Player) checks if a piece belongs to a player
% if the player is "B" then the piece must be between 1 and number_of_Pieces_per_Player
piece_owner(Piece,"B"):-
    piece(Piece),
    number_of_Pieces_per_Player(N),
    Piece=<N,
    !.

% if the player is "W" then the piece must be between number_of_Pieces_per_Player+1 and 2*number_of_Pieces_per_Player.
piece_owner(Piece,"W"):-
    piece(Piece),
    number_of_Pieces_per_Player(N),
    Piece=<2*N,
    Piece>N.

%%%%%%%%%% Piece Size %%%%%%%%%%%%%%%%
% piece_size(Piece,Size) checks the size of a piece
piece_size(Piece,Size):-
       piece(Piece),
       piece_size(Piece,0,0,Size).

% piece_size(Piece,Acc,Piece_Size,Size) - Auxiliary predicate to check the size of a piece.
% Acc is the number of pieces that have been checked so far. It goes through all the pieces until it finds the piece with id Piece, and returns the size of that piece.
piece_size(Piece,Acc,Piece_Size,Size):-
    (
        Piece=<Acc;
        (
            number_of_Pieces_per_Player(N),
            Piece>N,
            Piece =< N +Acc
        )
    ),
    Size is Piece_Size -1 .

% If it's not the piece we are looking for then it checks the next piece size
piece_size(Piece,Acc,Piece_Size,Size):-
    number_of_Pieces(Piece_Size,N),
    Acc2 is Acc +N,
    Piece_Size2 is Piece_Size +1, 
    piece_size(Piece, Acc2,Piece_Size2, Size).


%%%%%%%%%% Piece Value %%%%%%%%%%%%%%%%
% piece_value(Piece, Value) - checks the value of a piece
% it founds the size of the piece and then checks the value of the size
piece_value(Piece, Value):-
    piece_size(Piece,Size),
    size_value(Size,Value).    


%%%%%%%%%% Pieces with inferior size %%%%%%%%%%%%%%%%
% pieces_with_inferior_size(Size , N) gets the number of pieces with size inferior to Size
% There are 0 pieces with size inferior to 0
pieces_with_inferior_size(0, 0).

% It checks the number of pieces with size inferior to Size-1 and adds the number of pieces with size Size-1
pieces_with_inferior_size(Size, N):-
    Size2 is Size -1,
    number_of_Pieces(Size2,N2),
    pieces_with_inferior_size(Size2,N3),
    N is N2 + N3.


%%%%%5%%%%% First piece of size %%%%%%%%%%%%%%%%
% first_piece_of_size(Size, Player,Piece) gives the id of the first piece of size Size that belongs to Player
% If the player is "B" then the first piece of size Size is the piece with id N+1, because the first N pieces belong to "B"
first_piece_of_size(Size, "B",Piece):-
    pieces_with_inferior_size(Size, N),
    Piece is N +1.

% If the player is "W" then the first piece of size Size is the piece with id N2+1+number_of_Pieces_per_Player, because the first N pieces belong to "B" and the next N pieces belong to "W"
first_piece_of_size(Size, "W",Piece):-
    pieces_with_inferior_size(Size, N),
    number_of_Pieces_per_Player(N2),
    Piece is N + N2 +1.


%%%%%%%%%% Next piece not on board %%%%%%%%%%%%%%%%
% next_piece_not_on_board(First_Piece,Last_Piece ,Piece) gives the id of the first piece between First_Piece and Last_Piece that is not on the board
% Checks that the First_Piece is inferior to the Last_Piece, and that it is on the board. Then it checks the next piece. When it finds a piece that is not on the board it returns it.
next_piece_not_on_board(First_Piece,Last_Piece ,Piece):-
        First_Piece<Last_Piece,
        piece_position(First_Piece,_,_Pos),!,
        Next_Piece is First_Piece +1,
        next_piece_not_on_board( Next_Piece,Last_Piece,Piece).


% If the Piece is not on the board then it returns it.
next_piece_not_on_board(First_Piece,Last_Piece,First_Piece):-
    First_Piece<Last_Piece.


