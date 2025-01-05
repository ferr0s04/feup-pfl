% Main menu
menu :- 
    nl,
    nl,
    nl,
    write('======================='), nl,
    write('      BLACKSTONE       '), nl,
    write('======================='), nl,
    write('1. Play'), nl,
    write('2. Controls'), nl,
    write('3. How to Play'), nl,
    write('4. Exit'), nl,
    write('Choose an option (1-4): '),
    read(Choice),
    handle_menu_choice(Choice).

% Handle menu choices
handle_menu_choice(1) :- play_menu.
handle_menu_choice(2) :- show_controls, menu.
handle_menu_choice(3) :- how_to_play_menu.
handle_menu_choice(4) :- write('Goodbye!'), nl, halt.
handle_menu_choice(_) :- write('Invalid choice. Please try again.'), nl, menu.

% Play menu
play_menu :-
    nl,
    write('Select Game Variant:'), nl,
    write('1. Base Blackstone'), nl,
    write('2. Medium Churn Variant'), nl,
    write('3. High Churn Variant'), nl,
    write('4. Back'), nl,
    write('Choose an option (1-4): '),
    read(VariantOption),
    (   VariantOption == 4 -> menu
    ;   integer(VariantOption), variant_type(VariantOption, Variant) -> 
        select_game_type(Variant)
    ;   write('Invalid variant choice. Returning to play menu.'), nl, play_menu
    ).

% Select game type
select_game_type(Variant) :-
    nl,
    write('Select Game Type:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Back'), nl,
    write('Choose an option (1-4): '),
    read(GameTypeOption),
    (   GameTypeOption == 4 -> play_menu
    ;   integer(GameTypeOption), game_type(GameTypeOption, GameType) -> 
        (   GameType == pvc -> choose_computer_difficulty(Variant, GameType)
        ;   GameType == cvc -> choose_computer_difficulty(Variant, GameType)
        ;   select_board_size(Variant, GameType, none)
        )
    ;   write('Invalid game type. Returning to play menu.'), nl, play_menu
    ).

% Select computer difficulty
choose_computer_difficulty(Variant, GameType) :-
    nl,
    write('Select Computer Difficulty:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    write('3. Back'), nl,
    write('Choose an option (1-3): '),
    read(DifficultyOption),
    (   DifficultyOption == 3 -> play_menu
    ;   integer(DifficultyOption), difficulty(DifficultyOption, Difficulty) -> 
        select_board_size(Variant, GameType, Difficulty)
    ;   write('Invalid difficulty choice. Returning to play menu.'), nl, choose_computer_difficulty(Variant, GameType)
    ).

select_board_size(Variant, GameType, Difficulty) :-
    nl,
    write('Select Board Size:'), nl,
    write('1. 6x6'), nl,
    write('2. 8x8'), nl,
    write('3. 10x10'), nl,
    write('4. 12x12'), nl,
    write('5. Back'), nl,
    write('Choose an option (1-5): '),
    read(BoardSizeOption),
    (   BoardSizeOption == 5 -> play_menu
    ;   integer(BoardSizeOption), board_size(BoardSizeOption, Size) -> 
        select_current_player(Variant, GameType, Difficulty, Size)
    ;   write('Invalid board size. Returning to play menu.'), nl, select_board_size(Variant, GameType, Difficulty)
    ).

% Select stone color
select_current_player(Variant, GameType, Difficulty, Size) :-
    nl,
    write('Choose Your Stone Color:'), nl,
    write('1. Red'), nl,
    write('2. Blue'), nl,
    write('3. Back'), nl,
    write('Choose an option (1-3): '),
    read(ColorOption),
    (   ColorOption == 3 -> 
        play_menu
    ;   integer(ColorOption), stone_color(ColorOption, CurrentPlayer) -> 
        nl, write('Starting game...'), nl,
        create_game_config(Variant, GameType, Difficulty, Size, CurrentPlayer)
    ;   write('Invalid choice. Returning to color selection.'), nl, select_current_player(Variant, GameType, Difficulty, Size, CurrentPlayer)
    ).

% Map stone color option to color
stone_color(1, r).
stone_color(2, b).
stone_color(_, r) :- write('Invalid choice. Defaulting to Red.'), nl.

% Map variant option to type
variant_type(1, base).
variant_type(2, medium_churn).
variant_type(3, high_churn).
variant_type(_, base) :- write('Invalid choice. Defaulting to Base Blackstone.'), nl.

% Map game type option to type
game_type(1, pvp).
game_type(2, pvc).
game_type(3, cvc).
game_type(_, pvp) :- write('Invalid choice. Defaulting to Player vs Player.'), nl.

% Map difficulty option to difficulty level
difficulty(1, easy).
difficulty(2, hard).
difficulty(_, easy) :- write('Invalid choice. Defaulting to Easy.'), nl.

% Map board size option to size
board_size(1, 6).
board_size(2, 8).
board_size(3, 10).
board_size(4, 12).
board_size(_, 6) :- write('Invalid choice. Defaulting to 6x6.'), nl.

% Show controls
show_controls :-
    nl,
    write('Controls:'), nl,
    write('1. Enter moves in the format [FromRow, FromCol, ToRow, ToCol].'), nl,
    write('2. Use valid board coordinates for moves.'), nl,
    write('3. Press numbers as prompted for menu navigation.'), nl,
    nl,
    write('IMPORTANT: Always use . at the end of each entry'), nl,
    nl,
    write('Press any key to return to the main menu.'), nl,
    read(_).

% How to play menu
how_to_play_menu :-
    nl,
    write('How to Play:'), nl,
    write('1. Introduction'), nl,
    write('2. Basic Rules'), nl,
    write('3. Variants'), nl,
    write('4. Back'), nl,
    write('Choose an option (1-4): '),
    read(Option),
    handle_how_to_play_option(Option).

handle_how_to_play_option(1) :- show_introduction, how_to_play_menu.
handle_how_to_play_option(2) :- show_basic_rules, how_to_play_menu.
handle_how_to_play_option(3) :- variants_menu.
handle_how_to_play_option(4) :- menu.
handle_how_to_play_option(_) :- write('Invalid choice. Please try again.'), nl, how_to_play_menu.

% Show introduction
show_introduction :-
    nl,
    write('INTRODUCTION:'), nl,
    write('Blackstone is a strategic two-player game designed by Mark Steere in March 2024,'), nl,
    write('with Alek Erickson making notable contributions. The game is played on a square board'), nl,
    write('of any even size (6x6 or larger). The perimeter of the board is populated with red and'), nl,
    write('blue stones in specific starting patterns.'), nl,
    nl,
    write('The objective is to outmaneuver your opponent by blocking or removing their stones,'), nl,
    write('eventually leading to their elimination. Alternatively, achieving total annihilation'), nl,
    write('of all red and blue stones is a winning strategy.'), nl,
    nl.

% Show basic rules
show_basic_rules :-
    nl,
    write('BASIC RULES:'), nl,
    write('1. Each player controls stones of a single color (Red or Blue). Red always moves first.'), nl,
    write('2. On your turn, choose one of your stones and move it like a Chess queen (any number'), nl,
    write('   of squares along a straight, unobstructed line in any direction: horizontal,'), nl,
    write('   vertical, or diagonal).'), nl,
    write('3. After moving, place a neutral black stone on the square your stone moved from. This'), nl,
    write('   black stone acts as an obstacle for future moves.'), nl,
    write('4. If, after placing the black stone, any red or blue stones are fully surrounded by adjacent'), nl,
    write('   stones of any color and cannot move, those stones are removed from the board.'), nl,
    write('5. The game ends in one of the following scenarios:'), nl,
    write('   - A player loses if all of their stones are removed.'), nl,
    write('   - A player wins if their move eliminates all remaining red and blue stones.'), nl,
    nl.

% Variants menu
variants_menu :-
    nl,
    write('Variants:'), nl,
    write('1. Medium Churn Variant'), nl,
    write('2. High Churn Variant'), nl,
    write('3. Back'), nl,
    write('Choose an option (1-3): '),
    read(Option),
    handle_variant_option(Option).

handle_variant_option(1) :- show_medium_variant, variants_menu.
handle_variant_option(2) :- show_high_variant, variants_menu.
handle_variant_option(3) :- how_to_play_menu.
handle_variant_option(_) :- write('Invalid choice. Please try again.'), nl, variants_menu.

% Show medium churn variant
show_medium_variant :-
    nl,
    write('MEDIUM CHURN VARIANT:'), nl,
    write('If your move blocks any red or blue stones, remove those stones and all black'), nl,
    write('stones that contributed to the blocks.'), nl,
    nl.

% Show high churn variant
show_high_variant :-
    nl,
    write('HIGH CHURN VARIANT:'), nl,
    write('If your move blocks any red or blue stones, remove those stones and ALL black'), nl,
    write('stones from the board.'), nl,
    nl.

% Save game configuration
create_game_config(Variant, GameType, Difficulty, Size, CurrentPlayer) :-
    setup_board(Size, Board),
    GameConfig = game_config(variant(Variant), game_type(GameType), difficulty(Difficulty), size(Size), current_player(CurrentPlayer)),
    GameState = game_state(board(Board), current_player(CurrentPlayer), size(Size)),
    initial_state(GameConfig, GameState).
