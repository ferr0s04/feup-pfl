play :-
    setup_board(6),          % Initialize the board (default size 6x6)
    display_board(6),        % Show the initial board
    game_loop(r).            % Start with player `r`

% Game loop
game_loop(Player) :-
    write('Player '), write(Player), write(', make your move!'), nl,
    write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
    read([FromRow, FromCol, ToRow, ToCol]),
    (   integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        (   move(Player, FromRow, FromCol, ToRow, ToCol) ->
                (   check_win(Player) -> true
                ;   next_player(Player, NextPlayer),
                    game_loop(NextPlayer)
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop(Player)
        )
    ;   write('Invalid input. Please enter integers.'), nl,
        game_loop(Player)
    ).

% Switch to the next player
next_player(r, b).
next_player(b, r).

% Check if a player has lost
check_win(Player) :-
    \+ (board(Row, Col, Player), can_move(Row, Col)),
    write('Player '), write(Player), write(' has no moves left. You lose!'), nl.


:- dynamic board/3.  % Represents the board: board(Row, Col, Stone).
                     % Stone can be 'red', 'blue', or 'black'.

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

forall(between(1, Size, Row),
    forall(between(1, Size, Col),
        (   Condition1 -> Action1
        ;   Condition2 -> Action2
        ;   true
        )
    )
).

% Initialize the game board
setup_board(Size) :-
    retractall(board(_, _, _)),  % Clear any existing board
    Size >= 6,                   % Ensure board is at least 6
    Size mod 2 =:= 0,            % Ensure board size is even
    setup_perimeter(Size).       % Set up the initial perimeter stones

% Setup the initial perimeter stones
setup_perimeter(Size) :-
    setup_perimeter_rows(Size, 1).

setup_perimeter_rows(Size, Row) :-
    Row =< Size,
    setup_perimeter_cols(Size, Row, 1),
    NextRow is Row + 1,
    setup_perimeter_rows(Size, NextRow).
setup_perimeter_rows(_, _). % Base case: no more rows

setup_perimeter_cols(Size, Row, Col) :-
    Col =< Size,
    (   Row = 1, Col mod 2 =:= 0, Col < Size -> assertz(board(Row, Col, r))  % Red stones in first row, even positions except last
    ;   Row = Size, Col mod 2 =:= 1, Col > 1 -> assertz(board(Row, Col, r))  % Red stones in last row, odd positions except first
    ;   Col = 1, Row mod 2 =:= 1, Row > 1 -> assertz(board(Row, Col, b))     % Blue stones in first column, odd positions except first
    ;   Col = Size, Row mod 2 =:= 0, Row < Size -> assertz(board(Row, Col, b)) % Blue stones in last column, even positions except last
    ;   true  % Empty cells
    ),
    NextCol is Col + 1,
    setup_perimeter_cols(Size, Row, NextCol).
setup_perimeter_cols(_, _, _). % Base case: no more columns

% Display the board with grid
display_board(Size) :-
    write('   |'),
    display_column_labels(Size, 1),
    nl,
    write('---+---+---+---+---+---+---+'),  % Improved grid line format
    nl,
    display_rows(Size, 1).

% Helper to display column labels
display_column_labels(Size, Col) :-
    Col =< Size,
    write(' '), write(Col), write(' |'),
    NextCol is Col + 1,
    display_column_labels(Size, NextCol).
display_column_labels(_, _).  % Base case: no more columns to display

% Helper to display the grid line
display_grid_line(Size) :-
    Size > 0,
    write('---'),
    NextSize is Size - 1,
    display_grid_line(NextSize).
display_grid_line(0).

% Helper to display all rows
display_rows(Size, Row) :-
    Row =< Size,
    write(' '), write(Row), write(' |'),  % Row label formatting
    display_columns(Size, Row, 1),  % Display the cells in the row
    nl,  % Move to the next line after printing a row
    write('---+---+---+---+---+---+---+'),  % Grid line after each row
    nl,
    NextRow is Row + 1,
    display_rows(Size, NextRow).
display_rows(_, _).  % Base case: no more rows to display

% Helper to display a single row's columns
display_columns(Size, Row, Col) :-
    Col =< Size,
    (   board(Row, Col, Stone) ->
        (   Stone = black -> write(' X |')  % Render black as X with space for alignment
        ;   write(' '), write(Stone), write(' |')  % Render other stones with spacing
        )
    ;   write('   |')  % Render empty cells as . with spacing
    ),
    NextCol is Col + 1,
    display_columns(Size, Row, NextCol).
display_columns(_, _, _).  % Base case: no more columns to display

% Make a move for a player
move(Player, FromRow, FromCol, ToRow, ToCol) :-
    valid_move(Player, FromRow, FromCol, ToRow, ToCol),
    retract(board(FromRow, FromCol, Player)),
    assertz(board(ToRow, ToCol, Player)),
    assertz(board(FromRow, FromCol, black)),
    handle_removals,
    display_board(6).

% Check if the move is valid
valid_move(Player, FromRow, FromCol, ToRow, ToCol) :-
    board(FromRow, FromCol, Player),
    nonvar(Player), nonvar(FromRow), nonvar(FromCol), nonvar(ToRow), nonvar(ToCol),  % Guard against uninstantiated values
    queen_path_clear(FromRow, FromCol, ToRow, ToCol).

% Ensure path is clear for a chess queen move
queen_path_clear(FromRow, FromCol, ToRow, ToCol) :-
    DeltaRow is ToRow - FromRow,
    DeltaCol is ToCol - FromCol,
    (   DeltaRow =:= 0; DeltaCol =:= 0; abs(DeltaRow) =:= abs(DeltaCol) ) ->
        path_clear(FromRow, FromCol, DeltaRow, DeltaCol)
    ;   write('Invalid movement direction.'), nl, fail.

% Valid movement directions (horizontal, vertical, or diagonal)
valid_direction(DeltaRow, DeltaCol) :-
    between(-1, 1, DeltaRow),
    between(-1, 1, DeltaCol),
    \+ (DeltaRow =:= 0, DeltaCol =:= 0).  % Exclude "no movement"

% Check if the path is clear recursively
path_clear(Row, Col, 0, 0).
path_clear(Row, Col, DeltaRow, DeltaCol) :-
    nonvar(DeltaRow), nonvar(DeltaCol), % Ensure these are instantiated
    SignRow is sign(DeltaRow),
    SignCol is sign(DeltaCol),
    NextRow is Row + SignRow,
    NextCol is Col + SignCol,
    (   \+ board(NextRow, NextCol, _) -> % Check if the next position is clear
        NewDeltaRow is DeltaRow - SignRow,
        NewDeltaCol is DeltaCol - SignCol,
        path_clear(NextRow, NextCol, NewDeltaRow, NewDeltaCol)
    ;   write('Blocked at ['), write(NextRow), write(','), write(NextCol), write('].'), nl, fail
    ).

% Handle blocked stones (if applicable)
handle_removals :-
    write('Finding blocked stones...'), nl,
    findall([Row, Col, Player], blocked_stone(Row, Col, Player), StonesToRemove),
    remove_stones(StonesToRemove).

% Check if a stone is blocked
blocked_stone(Row, Col, Player) :-
    board(Row, Col, Player),
    nonvar(Row), nonvar(Col), nonvar(Player),  % Ensure inputs are instantiated
    \+ (valid_direction(DeltaRow, DeltaCol), % For all directions
        ToRow is Row + DeltaRow,
        ToCol is Col + DeltaCol,
        valid_move(Player, Row, Col, ToRow, ToCol)
    ),
    write('Stone is blocked.'), nl.

% Check if a stone can make any move
can_move(Row, Col) :-
    board(Row, Col, Player),
    write('Checking possible moves for stone at ['), write(Row), write(','), write(Col), write('] for player '), write(Player), nl,
    valid_direction(DeltaRow, DeltaCol),  % Generate potential move directions
    ToRow is Row + DeltaRow,
    ToCol is Col + DeltaCol,
    (   valid_move(Player, Row, Col, ToRow, ToCol) ->
        write('Can move to ['), write(ToRow), write(','), write(ToCol), write('].'), nl, !  % Cut after finding one valid move
    ;   fail
    ).

% Remove blocked stones
remove_stones([]).
remove_stones([[Row, Col, _] | Rest]) :-
    retract(board(Row, Col, _)),
    remove_stones(Rest).

% Remove all black stones (High Churn)
remove_all_black_stones :-
    retractall(board(_, _, B)).

% Remove contributing black stones (Medium Churn)
remove_contributing_black_stones(_) :-
    % Implementation of identifying contributing black stones goes here
    true.

% Winning condition
check_win(Player) :-
    \+ board(_, _, Player),
    write(Player), write(' loses!').

