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

(* Hash Table for the minimax algorithm. *)
let minimax_tbl = Hashtbl.create 1000

(* [generate_game_tree mvopt gs depth] makes a game tree. It takes in an move
 * list [mvopt], a game state [gs] and depth of the minimax algorithm [depth],
 * and generates a game tree based on all the informations.*)
let rec generate_game_tree mvopt gs depth : game_tree =
  let possible_moves = get_possible_moves gs.board gs.current_player in
  if (depth > 0) then
    (* Generates the subtrees of current tree based upon board and a player
     * passed in*)
    let generate_subtree mv new_gs =
      generate_game_tree (Some mv) new_gs (depth-1)
    in
    (* Makes subtree for each move in possible_move. *)
    let subtrees = List.fold_left
        (fun acc (to_mv,to_mvlst) -> ((generate_subtree (to_mv,to_mvlst)
          (make_move gs to_mv to_mvlst))::acc)) [] possible_moves
    in
      Node (mvopt ,gs, subtrees)
  else
    Leaf (gs)

(* [position_eval] evaluates every position on the board. The corners are worth
 * the highest. The pieces that can give up the corners are weighted negatively
 * edges are weighted slightly positvely.*)
let position_eval mv =
  let b = !size -1 in
  let bm = b - 1 in
  let corner_val = 40 in
  let adj_corner_val = -20 in
  let edge_val = 6 in
  let x = fst mv in
  let y = snd mv in
  if x=0 then
    if y=0 || y= b then corner_val
    else if y=bm then adj_corner_val
    else edge_val
  else if x=1 then
    if y=0 || y=bm || y=1 then adj_corner_val
    else if y =b then edge_val
    else 1
  else if x=bm then
    if y=bm || y=1 || y=0 then adj_corner_val
    else if y =b then edge_val
    else 1
  else if x=b then
    if y=0 || y=b then corner_val
    else if y=bm then adj_corner_val
    else edge_val
  else
    if y=b then edge_val
    else 1 
  
(* [eval_game_state gs] uses the heuristic function to evaluate the game state.
* It takes in a game state [gs] and evaluates the game state returning an int.
  The score weights number of possible moves, corner pieces, and tokens on edges
  positively and weights frontier tokens and tokens adjacent to corners negatively
  If it is late in the game. heavily weights coin parity*)
let eval_game_state gs : int =
  let ptok = gs.current_player.tok in
  let score = ref 0 in
  let b = gs.board in
  let turn_counter = ref 0 in
  let net_pieces = ref 0 in
  let net_frontier= ref 0 in
  (for i=0 to (!size -1) do
    for j=0 to (!size -1) do
      if b.(i).(j) = Empty then
        let is_move =  quick_is_valid_move b (i,j) gs.current_player in
        if is_move then score := !score + 10 else ()
      else if b.(i).(j) = ptok then
        let pos_eval = position_eval (i,j) in
        turn_counter := !turn_counter +1;
        net_pieces := !net_pieces +1;
        score := !score  + pos_eval ;
        if (is_frontier b (i,j)) then net_frontier := !net_frontier + 1
        else ()
      else
        let pos_eval = position_eval (i,j) in
        turn_counter := !turn_counter +1;
        net_pieces := !net_pieces -1;
        score := !score  - pos_eval;
        if (is_frontier b (i,j)) then net_frontier := !net_frontier - 1
        else ()
    done
  done);
  let score_end_game ()  = score := !score  + 200*(!net_pieces) in
  let score_late_game () = score := !score  + 25*(!net_pieces)  in
  (if !size = 8 then 
    if (!turn_counter) > 55 then score_end_game ()
    else if (!turn_counter) > 48 then score_late_game ()
    else ()
  else if !size = 6 then
    if (!turn_counter) > 28 then score_end_game ()
    else if (!turn_counter) > 25 then score_late_game ()
    else ()
  else 
    if (!turn_counter) > 9 then score_end_game ()
    else if (!turn_counter) > 6 then score_late_game ()
    else ());
  score:= !score - 5*(!net_frontier); 
  !score

(* [compute_minimax gt alpha beta depth maximizing] calculate a minimax score
 * for gt and all its subtrees and adds them to minimax_tbl. The minimax score is
 * calculated using [eval_game_state] if gt is a leaf. Otherwise it is assigned
 * the maximum or minimum score of its children depending upon maximizing.
 * alpha and beta are the pruning parameters. *)
let rec compute_minimax (gt:game_tree) alpha beta (depth:int) (maximizing:bool) =
  match gt with
  (* Calls heuristic function on leaf or when depth = 0. *)
  | Leaf gs -> Hashtbl.add minimax_tbl gt (eval_game_state gs);
  | Node (mvopt, gs, children) when depth = 0 ->
    Hashtbl.add minimax_tbl gt (eval_game_state gs);
  (* If Node and depth<>0, then pick the either the highest or lowest value
   * depending on maximizing. *)
  | Node (mvopt, gs,children) ->
    if (maximizing) then
      let best_value = ref alpha in
      (* Calculates maximum value of all the children and return it with gt
       * to form a minimax_tree. *)
      let rec calc_max_val lst =
        match lst with
        | [] -> Hashtbl.add minimax_tbl gt (!best_value)
        | h::t ->
          let () = compute_minimax h (!best_value) beta (depth-1) false in
          let child_val = Hashtbl.find minimax_tbl h in
          best_value := max (!best_value) child_val;
          if (!best_value) > beta then  (* Beta cut-off*)
            Hashtbl.add minimax_tbl gt beta
          else
            calc_max_val t
      in calc_max_val children
    else (* minimizing *)
      let best_value = ref beta in
      (* Calculates min value of all the children and return it with gt
       * to form a minimax_tree. *)
      let rec calc_min_val lst =
         match lst with
        | [] -> Hashtbl.add minimax_tbl gt !best_value
        | h::t ->
          let () = compute_minimax h alpha (!best_value) (depth-1) false in
          let child_val = Hashtbl.find minimax_tbl h in
          best_value := min (!best_value) child_val;
          if (!best_value) < alpha then (* alpha cut-off*)
            Hashtbl.add minimax_tbl gt (alpha)
          else
            calc_min_val t
      in calc_min_val children

(* [compute_minimax_move depth gs] takes in depth of minimax tree [depth] and
 * game state [gs], outputs move that maximizes the winning chance and minimizes
 * the losing chance. raises NoMove if there are no possible moves.
 * Precondition : depth > 1 *)
let compute_minimax_move depth gs =
  let play = gs.current_player in
  let gt= generate_game_tree None gs depth in
  let () = compute_minimax gt min_int max_int depth true in
  (* Minimax has been computed. Find the best move. *)
  match gt with
  | Node (mvopt, gs,children) ->
    let best_eval = Hashtbl.find minimax_tbl gt in
    let best_children = List.filter
      (fun ele -> (Hashtbl.find minimax_tbl ele) = best_eval) children in
    if List.length best_children = 0 then raise NoMove
    else
      let best_child = List.hd best_children in
      Hashtbl.clear minimax_tbl;
      (match best_child with
        | Node (Some x, gs,children) -> set_current_player play; x
        | _ -> failwith "depth 0 is not allowed")
  | Leaf gs -> failwith "depth 0 is not allowed"

(* [rand_move gs] takes in a game state [gs] and lets the AI picks a random move
 * of all the possible valid moves. Returns that move and the tokens to be
 * flipped if that move is made. raises NoMove if there are no possible moves.*)
let rand_move gs =
  (* if gs.current_player.ptype = Human then failwith "rand_move is called inapproriately"
  else *)
  let possible_moves = Board.get_possible_moves gs.board gs.current_player in
  print_endline "reached";
  let num_pmoves = (List.length possible_moves) in
  (* if there are no possible moves then fail for now*)
  if num_pmoves = 0 then raise NoMove
  else
    let rand_int = Random.int num_pmoves in
    List.nth possible_moves rand_int

(* The algorithm which AI is using. Defaults to rand_move
but is changed by initalize_difficulty*)
let ai_algorithm = ref rand_move

(* [initialize_difficulty cs] takes in a settings [cs] and based on the
 * difficulty provided by the settings, change the depth of the minimax
 * algorithm. *)
let initialize_difficulty cs =
  match cs.difficulty with
  | 0 -> ()
  | 1 -> ai_algorithm := (compute_minimax_move 2); ()
  | 2 -> ai_algorithm := (compute_minimax_move 3); ()
  | 3 -> ai_algorithm := (compute_minimax_move 4); ()
  | _ -> ()




