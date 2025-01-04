:- use_module(library(random)).
:- use_module('menu.pl').
:- use_module('board.pl').

% Entry point for the game
play :-
    menu.

% Define the initial game state
initial_state(GameConfig, GameState) :-
    GameConfig = game_config(variant(Variant), game_type(GameType), difficulty(Difficulty), size(Size), color(Color)),
    GameState = game_state(board(Board), current_player(Color), captured_pieces([])),
    display_game(GameState, GameType, Variant, Difficulty, Size).

% Adjusted game start logic to pass variants and difficulties
display_game(GameState, GameType, Variant, Difficulty, Size) :-
    GameState = game_state(board(Board), current_player(Color), captured_pieces(CapturedPieces)),
    display_board(Size, Board),
    (   GameType == pvp ->
        game_loop(Color, Size, Board, Variant)
    ;   GameType == pvc ->
        game_loop_pvc(Color, Size, Board, Variant, Difficulty)
    ;   GameType == cvc ->
        game_loop_cvc(Color, Size, Board, Variant, Difficulty)
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
            computer_move(b, Size, Board, IntermediateBoard, Difficulty),  
            handle_removals(IntermediateBoard, Variant, UpdatedBoard)
        (   check_win(Player, UpdatedBoard) -> true
        ;   game_loop_pvc(r, Size, UpdatedBoard, Variant, Difficulty)
        )
    ).

% Game loop for Computer vs Computer 
game_loop_cvc(Player, Size, Board, Variant, Difficulty) :-
    write('Computer '), write(Player), write(' is making a move...'), nl,
        computer_move(Player, Size, Board, IntermediateBoard, Difficulty),  
        handle_removals(IntermediateBoard, Variant, UpdatedBoard)
    (   check_win(Player, UpdatedBoard) -> true
    ;   next_player(Player, NextPlayer),
        game_loop_cvc(NextPlayer, Size, UpdatedBoard, Variant, Difficulty)
    ).

% Make a move for a player
move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard) :-
    valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board),
    update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, IntermediateBoard),
    handle_removals(IntermediateBoard, Variant, UpdatedBoard),
    display_board(Size, UpdatedBoard).

% Handle blocked stones based on game variant
handle_removals(Board, base, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), StonesToRemove),
    remove_stones(StonesToRemove, Board, UpdatedBoard).

handle_removals(Board, medium_churn, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), BlockedStones),
    remove_contributing_black_stones(BlockedStones, Board, UpdatedBoard).

handle_removals(Board, high_churn, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), BlockedStones),
    remove_all_black_stones(Board, TempBoard),
    remove_stones(BlockedStones, TempBoard, UpdatedBoard).

% Integration into the game loop
computer_move(Player, Size, Board, UpdatedBoard, Difficulty) :-
    (   Difficulty == easy ->
        random_computer_move(Player, Size, Board, UpdatedBoard)
    ;   Difficulty == hard ->
        greedy_computer_move(Player, Size, Board, UpdatedBoard)
    ).

% Easy mode computer move: Randomly selects a valid move
random_computer_move(Player, Size, Board, UpdatedBoard) :-
    findall(
        (FromRow, FromCol, ToRow, ToCol),
        (member((FromRow, FromCol, Player), Board),
         valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board)),
        Moves
    ),
    (   Moves = [] ->
        write('No valid moves available for player '), write(Player), nl,
        UpdatedBoard = Board  % No moves available
    ;   random_member((FromRow, FromCol, ToRow, ToCol), Moves),
        move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard)
    ).

% Hard mode computer move: Greedy approach to maximize immediate advantage
greedy_computer_move(Player, Size, Board, UpdatedBoard) :-
    findall(
        (FromRow, FromCol, ToRow, ToCol, Score),
        (member((FromRow, FromCol, Player), Board),
         valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board),
         evaluate_move(Player, FromRow, FromCol, ToRow, ToCol, Board, Score)),
        ScoredMoves
    ),
    (   ScoredMoves = [] ->
        write('No valid moves available for player '), write(Player), nl,
        UpdatedBoard = Board  % No moves available
    ;   max_member((FromRow, FromCol, ToRow, ToCol, _), ScoredMoves),
        write('Greedy move selected: '),
        write(FromRow), write(','), write(FromCol), write(' -> '),
        write(ToRow), write(','), write(ToCol), nl,
        move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard),
        display_board(Size, UpdatedBoard)
    ).

% Evaluate the "score" of a move based on immediate gains
evaluate_move(Player, FromRow, FromCol, ToRow, ToCol, Board, Score) :-
    % Simulate the move
    update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, TempBoard),
    % Count the blocked stones for the opponent after the move
    opponent(Player, Opponent),
    findall((Row, Col), blocked_stone(Row, Col, Opponent, TempBoard), BlockedStones),
    length(BlockedStones, Score).

% Utility to determine the opponent player
opponent(r, b).
opponent(b, r).

% Switch to the next player
next_player(r, b).
next_player(b, r).

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

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
path_clear(Row, Col, DeltaRow, DeltaCol, Board) :-
    SignRow is sign(DeltaRow),
    SignCol is sign(DeltaCol),
    NextRow is Row + SignRow,
    NextCol is Col + SignCol,
    NewDeltaRow is DeltaRow - SignRow,
    NewDeltaCol is DeltaCol - SignCol,
    \+ member((NextRow, NextCol, _), Board),  % Ensure the cell is unoccupied
    (NewDeltaRow =:= 0, NewDeltaCol =:= 0 -> true; path_clear(NextRow, NextCol, NewDeltaRow, NewDeltaCol, Board)).

% Check if a stone is blocked
blocked_stone(Row, Col, Player, Board) :-
    member((Row, Col, Player), Board),
    \+ (
        valid_direction(DeltaRow, DeltaCol), 
        DeltaRow \= 0; DeltaCol \= 0,  % Exclude (0,0)
        ToRow is Row + DeltaRow,
        ToCol is Col + DeltaCol,
        valid_move(Player, Row, Col, ToRow, ToCol, Board)
    ).

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
    member((Row, Col, _), Board), 
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
adjacent_to_blocked_stones((BlackRow, BlackCol), BlockedStones, _) :-
    member((BlockedRow, BlockedCol, _), BlockedStones),
    abs(BlackRow - BlockedRow) =< 1,
    abs(BlackCol - BlockedCol) =< 1.

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
    opponent(Player, Opponent),
    \+ (member((Row, Col, Player), Board), can_move(Row, Col, _, _, Board)),  % No moves for current player
    (   \+ (member((Row, Col, Opponent), Board), can_move(Row, Col, _, _, Board)) ->
        write('Both players are immobile. It’s a draw!'), nl
    ;   write('Player '), write(Player), write(' has no moves left. You lose!'), nl
    ).
