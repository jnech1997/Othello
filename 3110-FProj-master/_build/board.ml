open Player
open Settings

(** a move is int * int tuple representing a move that could
 * be made on the gameboard *)
type move = int * int

(** token matrix representing the current game board *)
type board = token array array

let size = ref 8

(** initializes the size of the board to a square matrix of [!size] x [!size] *)
let initialize_size cs =
  match cs.board_length with
  | 8 -> size := 8
  | 6 -> size := 6
  | 4 -> size := 4
  | _ -> ()

(* [init_board ()] generates a new game board. *)
let init_board () =
  let empty_board = Array.make_matrix (!size) (!size) Empty in
  empty_board.((!size - 2)/2).((!size - 2)/2) <- Black;
  empty_board.((!size - 2)/2 + 1).((!size - 2)/2 + 1) <- Black;
  empty_board.((!size - 2)/2).((!size - 2)/2 + 1) <- White;
  empty_board.((!size - 2)/2 + 1).((!size - 2)/2) <- White;
  empty_board

(* [is_on_board x y] checks whether the provided coordinate is on board. *)
let is_on_board x y =
  (x >= 0 && x <= (!size -1) && y >= 0 && y <= (!size -1))

(* [is_frontier b mv] returns whether a move would be a frontier disc on any
 * board [b]. A frontier disc is one adjacent to an empty square. *)
let is_frontier b mv  =
  if is_on_board (fst mv) (snd mv) then
  (let found_adj_space = ref false in
  let x = fst mv in
  let y = snd mv in
  (for dy = -1 to 1 do
    for dx = -1 to 1 do
      if (dy=0 && dx =0) then ()
      else
        let tempx = ref (x + dx) in
          let tempy = ref (y + dy) in
          if (not (is_on_board (!tempx) (!tempy))) then ()
          else
            if (b.(!tempx).(!tempy) = Empty) then
              found_adj_space := true
            else ()
    done
  done);
  !found_adj_space) else false

(* [quick_is_valid_move b mv p] takes in a board [b], a move [mv] and a player
 * [p] and checks whether a move is valid or not. It returns a boolean. *)
let quick_is_valid_move (b:board) (mv:move) (p:player)  =
  if (b.(fst mv).(snd mv) = Empty) then (
  let x = fst mv in
  let y = snd mv in
  let player_tok = p.tok in
  let opp_tok = other_tok player_tok in
  let orig_position = b.(x).(y) in
  (* Make the potential move on board. *)
  b.(x).(y) <- player_tok;
  let is_move = ref false in
  (for dy = -1 to 1 do
    for dx = -1 to 1 do
      if (dy = 0 && dx = 0) then ()
      else
        let tempx = ref (x + dx) in
        let tempy = ref (y + dy) in
        if ((not (is_on_board (!tempx) (!tempy)))
            || (b.(!tempx).(!tempy) = player_tok)) then ()
        else
            let on_board = ref true in
            (* Move in cardinal direction until an empty token
             * is reached or off the board. *)
            (while (!on_board && (b.(!tempx).(!tempy) = opp_tok)) do
              tempx := !tempx + dx;
              tempy := !tempy + dy;
              if (is_on_board !tempx !tempy) then () else on_board := false
            done);
          if (!on_board) then
              if (b.(!tempx).(!tempy) = player_tok) then
                is_move := true
              else ()
          else ()
    done
  done);
  (* Undo potential move. *)
  b.(x).(y) <- orig_position;
  !is_move)
  else false

(* [num_possible_moves b p] takes in a board [b] and a player [p], and returns
 * the number of possible moves the player can make. *)
let num_possible_moves b p =
  let num_moves= ref 0 in
  (for i=0 to (!size-1) do
    for j=0 to (!size-1) do
      if b.(i).(j) = Empty then
        let is_move = quick_is_valid_move b (i,j) p in
        if is_move then num_moves := !num_moves +1 else ()
      else ()
    done
  done);
  !num_moves

(* [is_valid_move b mv p] returns an list of positions that need their token
 * flipped if this move is valid. It returns an empty list when the move is not
 * valid. *)
let is_valid_move (b:board) (mv:move) (p:player)=
  let x = fst mv in
  let y = snd mv in
  let player_tok = p.tok in
  let opp_tok = other_tok player_tok in
  let orig_position = b.(x).(y) in
  (* Make potential move on board. *)
  b.(x).(y) <- player_tok;
  let to_flip = ref [] in
  (* Loop through all possible cardinal directions and check to see
   * if any tokens should be flipped. *)
  (for dy = -1 to 1 do
    for dx = -1 to 1 do
      if (dy=0 && dx =0) then ()
      else
        let tempx = ref (x + dx) in
        let tempy = ref (y + dy) in
        if (not (is_on_board !tempx !tempy)) then ()
        else
            let on_board = ref true in
            (* Move in cardinal direction until an empty token
             * is reached or off the board. *)
            (while (!on_board && (b.(!tempx).(!tempy) = opp_tok)) do
              tempx := !tempx + dx;
              tempy := !tempy + dy;
              if (is_on_board !tempx !tempy) then () else on_board := false
            done);
            if (!on_board) then
              if (b.(!tempx).(!tempy) = player_tok) then
              (* Found a valid move and will now move backwards to figure out
               * pieces to flip *)
                while ((!tempx <> x) || (!tempy <> y)) do
                  tempx := !tempx - dx;
                  tempy := !tempy - dy;
                  (* Printf.printf "in while loop for valid moves"; *)
                  if ((!tempx <> x) || (!tempy <> y)) then
                    to_flip := ((!tempx, !tempy)::(!to_flip))
                  else ()
                done
              else ()
            else ()
    done
  done);
  (* Undo potential move. *)
  b.(x).(y) <- orig_position;
  !to_flip


(* [get_possible_moves b p] takes in a board [b] and the current player [p],
 * outputs a list of all possible moves that player could make. *)
let get_possible_moves b p =
  let valid_move_lst = ref [] in
  (for i=0 to (!size-1) do
    for j=0 to (!size-1) do
      if b.(i).(j) = Empty then
        let to_flip = is_valid_move b (i,j) p in
        if((List.length to_flip) >0)
          then valid_move_lst := (((i,j),to_flip)::(!valid_move_lst))
        else ()
      else ()
    done
  done);
  (* let possible_moves = ((List.length !(valid_move_lst)) >0) in
  (possible_moves,!valid_move_lst) *)
 !valid_move_lst

(* [is_board_full b] returns a boolean indicating whether the board [b] is full
 * or not. *)
let is_board_full b =
  let result = ref true in
  (for i = 0 to (!size-1) do
    for j = 0 to (!size-1) do
      if b.(i).(j) = Empty then
        result := false
      else ()
    done
  done);
  !result
