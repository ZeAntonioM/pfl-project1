number_of_Pieces_per_Player(15).

% Piece(id) - Check if the Piece exists
piece(Id):-
    number_of_Pieces_per_Player(N),
    Id>0,
    Id=<N*2.

% Number_of_Pieces(Piece_Size, Number_of_existing_pieces) - The number of existing pieces (per player) of each size
number_of_Pieces(3,5).
number_of_Pieces(4,4).
number_of_Pieces(5,3).
number_of_Pieces(6,2).
number_of_Pieces(7,1).
number_of_Pieces(_,0).

% size_value(Size,Value) - Value corresponding to the size of the piece
size_value(3,1).
size_value(4,2).
size_value(5,3).   
size_value(6,4).                                    
size_value(7,6).        

                

% piece_owener(Piece,Player) - Piece belongs to Player
piece_owener(Piece,"A"):-
    piece(Piece),
    number_of_Pieces_per_Player(N),
    Piece=<N,
    !.

piece_owener(Piece,"B"):-
    piece(Piece),
    number_of_Pieces_per_Player(N),
    Piece=<2*N.


    
% piece_size(Piece,Size) - Size of the Piece
piece_size(Piece,Size):-
       piece(Piece),
       pice_size(Piece,0,0,Size).

pice_size(Piece,Acc,Piece_Size,Size):-
    (
        Piece=<Acc;
        (
            number_of_Pieces_per_Player(N),
            Piece>N,
            Piece =< N +Acc
        )
    ),
    Size is Piece_Size -1 .


pice_size(Piece,Acc,Piece_Size,Size):-
    number_of_Pieces(Piece_Size,N),
    Acc2 is Acc +N,
    Piece_Size2 is Piece_Size +1, 
    pice_size(Piece, Acc2,Piece_Size2, Size).

% piece_value(Piece, Value) - Value of the piece
piece_value(Piece, Value):-
    piece_size(Piece,Size),
    size_value(Size,Value).    

% pieces_with_inferior_size(Size , N) - Number of pieces with lower size than Size
pieces_with_inferior_size(0, 0).
pieces_with_inferior_size(Size, N):-
    Size2 is Size -1,
    number_of_Pieces(Size2,N2),
    pieces_with_inferior_size(Size2,N3),
    N is N2 + N3.

% first_piece_of_size(Size, Player,Piece):- The first Piece with size Size from the player Player
first_piece_of_size(Size, "A",Piece):-
    pieces_with_inferior_size(Size, N),
    Piece is N +1.

first_piece_of_size(Size, "B",Piece):-
    pieces_with_inferior_size(Size, N),
    number_of_Pieces_per_Player(N2),
    Piece is N + N2 +1.



