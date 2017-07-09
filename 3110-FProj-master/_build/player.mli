open Settings

(* Variant representing a token on the game board. *)
type token = Black | White | Empty

(* Variant representing the type of a player. *)
type player_type = Human | AI

(* Variant representing who goes first and who goes second. *)
type order = First | Second

(* Record representing the type of player and whether they goes first
 * or second. *)
type player = {ptype : player_type; tok : token; turn : order}

(* Player1 and player2 represent references to the two players in the game.
 * The order they are created in does not matter. *)
val player1: player option ref
val player2: player option ref

(* Reference to the current player. *)
val current_player_ref: player option ref

(* [set_p1] sets player1's value to a player value. *)
val set_p1: player -> unit

(* [set_p2] sets player2's value to a player value. *)
val set_p2: player -> unit

(* [initialize_players] takes in the current settings from Settings and
 * initializes the players. *)
val initialize_players: settings -> unit

(* [get_current_player] returns the current player who's playing the game. *)
val get_current_player: unit -> player

(* [get_next_player] returns the next player who's playing the game. *)
val get_next_player: unit -> player

(* [set_current_player p] sets the current player's value to a player [p]. *)
val set_current_player: player -> unit

(* [next_player] sets current_player_ref equal to the next_player. *)
val switch_player: unit -> unit

(* [other_tok] returns the other token of the current token. Failwith
 * "There's no other token" if the input token is Empty. *)
val other_tok: token -> token

(* [extract_from_option] extracts the value of a player option. Failwith
 * "next_player called when none made" when the player option has None. *)
val extract_from_option: player option -> player