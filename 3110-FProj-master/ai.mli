open Game
open Board
open Player
open Settings

(*raised when by rand_move and compute_minimax_move when AI has no possible moves*)
exception NoMove

(* Variant representing a game_tree. Showing a move that made the gamestate, and
 * a list of child game_tree for every possible move on the game_state. *)
type game_tree =
  | Leaf of game_state
  | Node of (move * move list) option * game_state * game_tree list

(* [compute_minimax] calculate a minimax score for gt and all its subtrees and 
 * adds them to minimax_tbl. The minimax score is calculated using 
 * [eval_game_state] if gt is a leaf. Otherwise it is assigned 
 * the maximum or minimum score of its children depending upon maximizing. 
 * alpha and beta are the pruning parameters. *)
val compute_minimax_move: int -> game_state -> move * move list

(* [eval_game_state] uses the heuristic function to evaluate the game state.
 * It takes in a game state and evaluates the game state. *)
val eval_game_state: game_state -> int

(* [rand_move] takes in a game state and lets the AI picks a random move 
 * of all the possible valid moves. Returns that move and the tokens to be 
 * flipped if that move is made. *)
val rand_move: game_state -> (move * move list)

(* The algorithm which AI is using. *)
val ai_algorithm: (game_state -> (move * move list)) ref

(* [initialize_difficulty] takes in a settings and based on the difficulty 
 * provided by the settings, change the depth of the minimax algorithm. *)
val initialize_difficulty: settings -> unit


