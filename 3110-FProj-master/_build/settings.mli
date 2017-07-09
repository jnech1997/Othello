
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
val current_settings: settings

(* [change_who_goes] changes the who_goes field of current settings to the input
 * value. *)
val change_who_goes: string -> unit

(* [change_players] changes the players field of current settings to be
 * the input value. *)
val change_players: (string * string) -> unit

(* [change_difficulty] changes the difficulty field of current settings to
 * the input value. *)
val change_difficulty: int -> unit

(** [change_board_size] changes the size of the board to [input] X [input]
 * Precondition: [input] is either 4, 6, or 8 *)
val change_board_size: int -> unit

(* change the images of the pieces *)
val change_image: string * string -> unit