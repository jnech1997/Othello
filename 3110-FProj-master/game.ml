open Player
open Board
open Settings

(* Record representing the state of the board: the current player
 * and the current board. *)
type game_state = {current_player: player; board: board}

(* [is_game_over gs cp np] returns a boolean whether the game is over. It takes
 * in a game state [gs], current player [cp] and next player [np].
 * The game is over when no one can move, or the board is full. *)
let is_game_over gs cp np =
  (get_possible_moves gs.board cp) = [] &&
  (get_possible_moves gs.board np) = [] ||
  (is_board_full gs.board)

(* [winning_player gs] is called only when is_game_over outputs true. It takes
 * in a game state [gs] and returns the winning player's token. If it's a draw
 * then return Empty. *)
let winning_player gs =
  let bt = ref 0 in let wt = ref 0 in
  for i = 0 to (!size -1) do
    for j = 0 to (!size -1) do
      if gs.board.(i).(j) = Empty then ()
      else if gs.board.(i).(j) = Black then bt := !bt + 1
      else wt := !wt + 1
    done
  done ;
  if bt = wt then Empty
  else if (bt > wt) then Black
  else White

(* A function that returns a new copy of the input matrix
 * copied from https://caml.inria.fr/resources/doc/faq/core.en.html
 * [copy_matrix m] makes a copy of a matrix. *)
let copy_matrix m =
  let l = Array.length m in
  if l=0 then m else
    let result = Array.make l m.(0) in
    for i=0 to l-1 do
      result.(i) <- Array.copy m.(i)
    done;
    result

(* [make_move gs mv mvlst] returns the game state after all the tokens in
 * [mvlst] have been flipped when a move [mv] is made in the input game state
 * [gs]. *)
let make_move gs mv mvlst =
  let ptok = gs.current_player.tok in
  let rec move_maker lst b=
    match lst with
    | [] -> b
    | h::t ->
      b.(fst h).(snd h) <- ptok; move_maker t b
  in
  let board_copy = copy_matrix gs.board in
  let new_board = move_maker (mv::mvlst) board_copy in
  {current_player = (switch_player (); get_current_player ()); board=new_board}



