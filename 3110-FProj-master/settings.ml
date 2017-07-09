(* Settings for othello game that user can change from the GUI menu. *)
type settings = {mutable who_goes: string;
                mutable players: (string * string);
                mutable difficulty: int;
                mutable board_length: int;
                mutable player1_image: string;
                mutable player2_image: string}

(* Starting settings for the game if the user does not change them.
 * This record will be changed when a user edits the settings in the
 * GUI menu. *)
let current_settings = {who_goes = "human";
                      players = ("human", "AI");
                      difficulty = 1;
                      board_length = 8;
                      player1_image = "b_circle.xpm";
                      player2_image = "w_circle.xpm"}


(* [change_who_goes input] changes the who_goes field of current settings to the
 * input value [input].
 * Precondition: input value is "human1", "human 2", or "AI". *)
let change_who_goes input =
  current_settings.who_goes <- input

(* [change_players input] changes the players field of current settings to be
 * the input value [input].
 * First player in the tuple has black token, second has white.
 * Precondition: the strings in the tuple can only be "human1", "human2",
 * or "AI", any human comes before AI in the tuple and human1 comes
 * before human2. *)
let change_players input =
  current_settings.players <- input

(* [change_difficulty input] changes the difficulty field of current settings to
 * the input value [input].
 * Precondition: the integer represents a valid difficulty. *)
let change_difficulty input =
  current_settings.difficulty <- input

(* [change_board_size input] changes the size of the board to [input] X [input]
 * Precondition: [input] is either 4, 6, or 8 *)
let change_board_size input =
  current_settings.board_length <- input

(* [change_image input] changes the images of the pieces *)
let change_image input =
  current_settings.player1_image <- fst input;
  current_settings.player2_image <- snd input
