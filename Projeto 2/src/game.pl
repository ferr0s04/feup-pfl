:- use_module(library(random)).
:- use_module('menu.pl').
:- use_module('board.pl').

% Entry point for the game
main :-
    menu.

% Adjusted game start logic to pass variants and difficulties
start_game(Variant, GameType, Difficulty, Size) :-
    setup_game(Size, Board),
    display_game(Size, Board),
    (   GameType == pvp ->
        game_loop(r, Size, Board, Variant)
    ;   GameType == pvc ->
        game_loop_pvc(r, Size, Board, Variant, Difficulty)
    ;   GameType == cvc ->
        game_loop_cvc(r, Size, Board, Variant, Difficulty)
    ).

% Game loop for Player vs Player
game_loop(Player, Size, Board, Variant) :-
    write('Player '), write(Player), write(', make your move!'), nl,
    write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
    read([FromRow, FromCol, ToRow, ToCol]),
    (   integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, IntermediateBoard) ->
                (   handle_removals(IntermediateBoard, Variant, UpdatedBoard),
                    (   check_win(Player, UpdatedBoard) -> true
                    ;   next_player(Player, NextPlayer),
                        game_loop(NextPlayer, Size, UpdatedBoard, Variant)
                    )
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop(Player, Size, Board, Variant)
        )
    ;   write('Invalid input. Please enter integers.'), nl,
        game_loop(Player, Size, Board, Variant)
    ).


% Game loop for Player vs Computer
game_loop_pvc(Player, Size, Board, Variant, Difficulty) :-
    (   Player = r ->
        write('Player r, make your move!'), nl,
        write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
        read([FromRow, FromCol, ToRow, ToCol]),
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, IntermediateBoard) ->
                (   handle_removals(IntermediateBoard, Variant, UpdatedBoard),
                    (   next_player(Player, Opponent),
                        check_win(Opponent, UpdatedBoard) -> true
                    ;   game_loop_pvc(b, Size, UpdatedBoard, Variant, Difficulty)
                    )
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop_pvc(Player, Size, Board, Variant, Difficulty)
        )
    ;   write('Computer is making a move...'), nl,
        (   Difficulty == hard ->
            write('Hard bot logic is not implemented yet.'), nl,
            UpdatedBoard = Board  % Placeholder: leave board unchanged
        ;   computer_move(b, Size, Board, IntermediateBoard),  % Easy bot logic
            handle_removals(IntermediateBoard, Variant, UpdatedBoard)
        ),
        (   check_win(Player, UpdatedBoard) -> true
        ;   game_loop_pvc(r, Size, UpdatedBoard, Variant, Difficulty)
        )
    ).

% Game loop for Computer vs Computer 
game_loop_cvc(Player, Size, Board, Variant, Difficulty) :-
    write('Computer '), write(Player), write(' is making a move...'), nl,
    (   Difficulty == hard ->
        write('Hard bot logic is not implemented yet.'), nl,
        UpdatedBoard = Board  % Placeholder: leave board unchanged
    ;   computer_move(Player, Size, Board, IntermediateBoard),  % Easy bot logic
        handle_removals(IntermediateBoard, Variant, UpdatedBoard)
    ),
    (   check_win(Player, UpdatedBoard) -> true
    ;   next_player(Player, NextPlayer),
        game_loop_cvc(NextPlayer, Size, UpdatedBoard, Variant, Difficulty)
    ).

% Computer move logic using can_move/2
computer_move(Player, Size, Board, UpdatedBoard) :-
    % Try to find a stone for the computer to move
    (   member((FromRow, FromCol, Player), Board),  % Find a stone controlled by the player
        can_move(FromRow, FromCol, ToRow, ToCol, Board)  % Check if the stone can move
    ->  % If there is a valid move
        write('Computer moved from ['), write(FromRow), write(','), write(FromCol),
        write('] to ['), write(ToRow), write(','), write(ToCol), write('].'), nl,
        move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard),  % Apply the move on the board
        display_board(Size, UpdatedBoard)  % Display the updated board
    ;   write('No valid moves available for player '), write(Player), nl,
        UpdatedBoard = Board  % No moves available, return the board unchanged
    ).


% Handle removals based on game variant
handle_removals(Board, base, UpdatedBoard) :-
    handle_removals(Board, UpdatedBoard).

handle_removals(Board, medium_churn, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), BlockedStones),
    remove_contributing_black_stones(BlockedStones, Board, UpdatedBoard).

handle_removals(Board, high_churn, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), BlockedStones),
    remove_all_black_stones(Board, TempBoard),
    remove_stones(BlockedStones, TempBoard, UpdatedBoard).


% Switch to the next player
next_player(r, b).
next_player(b, r).

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

% Make a move for a player
move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard) :-
    valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board),
    update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, IntermediateBoard),
    handle_removals(IntermediateBoard, UpdatedBoard),
    display_game(Size, UpdatedBoard).

% Update the board after a move
update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, UpdatedBoard) :-
    replace(Board, (FromRow, FromCol, Player), (FromRow, FromCol, black), TempBoard),
    replace(TempBoard, (ToRow, ToCol, black), (ToRow, ToCol, Player), UpdatedBoard).

% Replace an element in the board
replace([Old | Rest], Old, New, [New | Rest]).
replace([Other | Rest], Old, New, [Other | UpdatedRest]) :-
    Other \= Old,
    replace(Rest, Old, New, UpdatedRest).


% Check if the move is valid
valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board) :-
    member((FromRow, FromCol, Player), Board),
    queen_path_clear(FromRow, FromCol, ToRow, ToCol, Board).


% Ensure path is clear for a chess queen move
queen_path_clear(FromRow, FromCol, ToRow, ToCol, Board) :-
    DeltaRow is ToRow - FromRow,
    DeltaCol is ToCol - FromCol,
    (   DeltaRow =:= 0; DeltaCol =:= 0; abs(DeltaRow) =:= abs(DeltaCol) ) ->
        path_clear(FromRow, FromCol, DeltaRow, DeltaCol, Board)
    ;   write('Invalid movement direction.'), nl, fail.

% Valid movement directions (horizontal, vertical, or diagonal)
valid_direction(DeltaRow, DeltaCol) :-
    between(-1, 1, DeltaRow),
    between(-1, 1, DeltaCol),
    \+ (DeltaRow =:= 0, DeltaCol =:= 0).  % Exclude "no movement"

% Check if the path is clear recursively
path_clear(Row, Col, 0, 0, _).
path_clear(Row, Col, DeltaRow, DeltaCol, Board) :-
    SignRow is sign(DeltaRow),
    SignCol is sign(DeltaCol),
    NextRow is Row + SignRow,
    NextCol is Col + SignCol,
    (   \+ member((NextRow, NextCol, _), Board) ->
        NewDeltaRow is DeltaRow - SignRow,
        NewDeltaCol is DeltaCol - SignCol,
        path_clear(NextRow, NextCol, NewDeltaRow, NewDeltaCol, Board)
    ;   write('Blocked at ['), write(NextRow), write(','), write(NextCol), write('].'), nl, fail
    ).

% Handle blocked stones (if applicable)
handle_removals(Board, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), StonesToRemove),
    remove_stones(StonesToRemove, Board, UpdatedBoard).

% Check if a stone is blocked
blocked_stone(Row, Col, Player, Board) :-
    member((Row, Col, Player), Board),
    \+ (valid_direction(DeltaRow, DeltaCol),
        ToRow is Row + DeltaRow,
        ToCol is Col + DeltaCol,
        valid_move(Player, Row, Col, ToRow, ToCol, Board)
    ).
    write('Stone is blocked.'), nl.

% Check if a stone can move
can_move(Row, Col, ToRow, ToCol, Board) :-
    member((Row, Col, Player), Board),
    valid_direction(DeltaRow, DeltaCol),
    ToRow is Row + DeltaRow,
    ToCol is Col + DeltaCol,
    valid_move(Player, Row, Col, ToRow, ToCol, Board).

% Remove blocked stones from the board
remove_stones([], Board, Board).
remove_stones([(Row, Col, _) | Rest], Board, UpdatedBoard) :-
    delete(Board, (Row, Col, _), TempBoard),
    remove_stones(Rest, TempBoard, UpdatedBoard).

% Remove contributing black stones (Medium Churn Variant)
remove_contributing_black_stones(BlockedStones, Board, UpdatedBoard) :-
    findall(
        (Row, Col),
        (member((Row, Col, black), Board),
         adjacent_to_blocked_stones((Row, Col), BlockedStones, Board)),
        ContributingBlackStones
    ),
    subtract(Board, ContributingBlackStones, TempBoard),
    subtract(TempBoard, BlockedStones, UpdatedBoard).

% Check if a black stone is adjacent to any blocked stone
adjacent_to_blocked_stones((BlackRow, BlackCol), BlockedStones, Board) :-
    member((BlockedRow, BlockedCol, _), BlockedStones),
    abs(BlackRow - BlockedRow) =< 1,
    abs(BlackCol - BlockedCol) =< 1,
    member((BlackRow, BlackCol, black), Board).

% Remove all black stones (High Churn Variant)
remove_all_black_stones(Board, UpdatedBoard) :-
    exclude(is_black_stone, Board, UpdatedBoard).

% Helper to identify black stones
is_black_stone((_, _, black)).

% Utility to delete an element from the board
delete([Elem | Rest], Elem, Rest).
delete([Other | Rest], Elem, [Other | UpdatedRest]) :-
    Other \= Elem,
    delete(Rest, Elem, UpdatedRest).

% Check win condition
check_win(Player, Board) :-
    \+ (member((Row, Col, Player), Board), can_move(Row, Col, _, _, Board)),  % No valid moves for player
    write('Player '), write(Player), write(' has no moves left. You lose!'), nl.
