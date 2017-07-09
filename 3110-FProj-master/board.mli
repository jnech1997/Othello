open Player
open Settings

(* A move is int * int tuple representing a move that could
 * be made on the gameboard *)
type move = int * int

(** token matrix representing the current game board *)
type board = token array array

val is_frontier: board -> move -> bool

(* [is_valid_move] returns an list of positions that need their token
 * flipped if this move is valid. It returns an empty list when the move is not
 * valid. *)
val is_valid_move: board -> move -> player ->  move list

(* [get_possible_moves] takes in a board and the current player, outputs a list
 * of all possible moves that player could make. *)
val get_possible_moves: board -> player -> ( ((move * move list) list))

(* [is_board_full] returns a boolean indicating whether the board is full
 * or not. *)
val is_board_full: board -> bool

(* [init_board] generates a new game board. *)
val init_board: unit -> board

(* The number of rows and cols. *)
val size: int ref

(* [quick_is_valid_move] takes in a board, a move and a player and checks
 * whether a move is valid or not for this player. It returns a boolean. *)
val quick_is_valid_move: board -> move -> player -> bool

(* [num_possible_moves] determines the total number of possible moves
 * that can be made by a player on this board *)
val num_possible_moves: board -> player -> int

(** initializes the size of the board to a square matrix of [!size] x [!size] *)
val initialize_size: settings -> unit
