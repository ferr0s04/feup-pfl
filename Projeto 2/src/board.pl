:- dynamic board/3.

% Initialize the game board
setup_board(Size) :- 
    retractall(board(_, _, _)), 
    Size >= 6, 
    Size mod 2 =:= 0, 
    setup_perimeter(Size).

% Setup the initial perimeter stones
setup_perimeter(Size) :- 
    setup_perimeter_rows(Size, 1).

setup_perimeter_rows(Size, Row) :- 
    Row =< Size, 
    setup_perimeter_cols(Size, Row, 1), 
    NextRow is Row + 1, 
    setup_perimeter_rows(Size, NextRow).
setup_perimeter_rows(_, _). 

setup_perimeter_cols(Size, Row, Col) :- 
    Col =< Size, 
    (   Row = 1, Col mod 2 =:= 0, Col < Size -> assertz(board(Row, Col, r))
    ;   Row = Size, Col mod 2 =:= 1, Col > 1 -> assertz(board(Row, Col, r))
    ;   Col = 1, Row mod 2 =:= 1, Row > 1 -> assertz(board(Row, Col, b))
    ;   Col = Size, Row mod 2 =:= 0, Row < Size -> assertz(board(Row, Col, b))
    ;   true
    ),
    NextCol is Col + 1,
    setup_perimeter_cols(Size, Row, NextCol).
setup_perimeter_cols(_, _, _).

% Display the board with grid
display_board(Size) :- 
    write('   |'), 
    display_column_labels(Size, 1),
    nl, 
    display_grid_line(Size), 
    nl, 
    display_rows(Size, 1).

% Helper to display column labels
display_column_labels(Size, Col) :- 
    Col =< Size, 
    write(' '), write(Col), write(' |'), 
    NextCol is Col + 1, 
    display_column_labels(Size, NextCol).
display_column_labels(_, _).

% Helper to display the grid line
display_grid_line(Size) :- 
    Size > 0, 
    write('---+'), 
    NextSize is Size - 1, 
    display_grid_line(NextSize).
display_grid_line(0) :- write('---').

% Helper to display all rows
display_rows(Size, Row) :- 
    Row =< Size, 
    write(' '), write(Row), write(' |'), 
    display_columns(Size, Row, 1),
    nl, 
    display_grid_line(Size),
    nl, 
    NextRow is Row + 1, 
    display_rows(Size, NextRow).
display_rows(_, _).

% Helper to display a single row's columns
display_columns(Size, Row, Col) :- 
    Col =< Size, 
    (   board(Row, Col, Stone) -> 
        (   Stone = black -> write(' X |') 
        ;   write(' '), write(Stone), write(' |')
        )
    ;   write('   |')
    ),
    NextCol is Col + 1, 
    display_columns(Size, Row, NextCol).
display_columns(_, _, _).
