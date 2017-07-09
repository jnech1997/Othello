open Player
open Board
open Settings

(* Record representing the state of the board: the current player
 * and the current board. *)
type game_state = {current_player: player; board: board}

(* [is_game_over] returns a boolean whether the game is over.
 * The game is over when no one can move, or the board is full. *)
val is_game_over: game_state -> player -> player -> bool

(* [winning_player] is called only when is_game_over outputs true. It takes
 * in a game state and returns the winning player's token. If it's a
 * draw then return Empty. *)
val winning_player: game_state -> token

(* [make_move] returns the game state after all the tokens which need to be
 * flipped have been flipped when a move is made in the input game state. *)
val make_move: game_state -> move -> move list -> game_state





