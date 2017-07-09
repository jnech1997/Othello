open GMain
open GdkKeysyms
open Player
open Board
open Game
open Settings
open Ai

let p1_image = ref "b_circle.xpm"
let p2_image = ref "w_circle.xpm"

(** Initialize the images that represent player1 and player2 on the GUI given
 * the current settings [cs] *)
let initialize_images cs : unit =
  match cs.player1_image, cs.player2_image with
  | "b_circle.xpm", "w_circle.xpm" -> p1_image := "b_circle.xpm"; p2_image := "w_circle.xpm"
  | "clarkson.xpm", "vader.xpm" -> p1_image := "clarkson.xpm"; p2_image := "vader.xpm"
  | "fire.xpm", "water.xpm" -> p1_image := "fire.xpm"; p2_image := "water.xpm"
  | _ -> ()

(** [button_table] is a hashtable of all the coordinates (int*int)
 * in the table (keys) and the buttons that correspond to them (values) *)
let button_table : (move, GButton.button) Hashtbl.t = (Hashtbl.create 64)

(** [is_filled] returns true if GButton [b] has an image in it *)
let is_filled b : bool = try (b#child;true) with | Gpointer.Null -> false

(** Create a new hbox with an image packed into it
 * and pack the box. Taken from LablGtk tutorial:
 * http://tinyurl.com/zskf6jt *)
let xpm_label_box ~file ~packing () : unit =
  if not (Sys.file_exists file) then failwith (file ^ "does not exist");
  (* create box for image and label and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~packing:(box#pack ~padding:3 ~expand:true) (); ()

(** [paint_button] paints the button at location [loc] with color [color] *)
let rec paint_button loc color : unit =
  let button = (Hashtbl.find button_table loc) in
  match color with
  | Black -> xpm_label_box ~file:(!p1_image) ~packing:button#add ()
  | White -> xpm_label_box ~file:(!p2_image) ~packing:button#add ()
  | Empty -> ()

(** [flip_buttons] takes in a list of (int*int) tuples [lst],
 * whose turn it is [turn], and flips buttons in the gui so that the buttons
 * have the color of the player whose turn it is *)
let rec flip_buttons lst turn : unit =
  match lst with
  | [] -> ()
  | h::t ->
    let button = (Hashtbl.find button_table h) in
    button#remove (button#child);
    (match turn with
    | Black ->
      xpm_label_box ~file:(!p1_image) ~packing:button#add ();
      flip_buttons t Black
    | White ->
      xpm_label_box ~file:(!p2_image) ~packing:button#add ();
      flip_buttons t White
    | Empty -> ())

(** [get_flips] returns list of pieces to flip
 * if [loc] is a possible move, else returns the empty list *)
let rec get_flips loc lst : move list =
  match lst with
  | [] -> []
  | h::t -> if fst h = loc then snd h else get_flips loc t

(** Must be in gamegui.ml because it calls other local functions.
 * Makes the AI's first move on the gui if it moves first in the game, all other moves
 * the AI makes will be called as a result of the user making a move. Takes in
 * the current player [cp], the game state [gs],
 * the ai_function [ai_fun], and the turn notifier button [tnb] *)
let ai_first_move cp gs ai_fun tnb : unit =
  match cp.ptype, cp.tok, cp.turn with
  | AI, Black, First ->
    let ((x,y), flips) = ai_fun !gs in
      paint_button (x,y) Black; (* paint the AI's first move, always Black *)
      flip_buttons flips Black; (* flip the necessary pieces based on AI's move *)
      gs := (make_move !gs (x,y) flips); (* update the game state *)
      tnb#remove (tnb#child); (* change the turn notification to prompt user *)
      xpm_label_box ~file:"whitesturn.xpm" ~packing:tnb#add ()
  | _ -> ()

(** Must be in gamegui.ml because it calls other local functions.
 * [make_ai_move] updates gamestate [gs] to reflect the AI's move given by
 * [ai_fun] and paints the AI's move on the gui with color [color] *)
let rec make_ai_move gs color ai_fun : unit =
  if (get_current_player ()).ptype = AI then
    let ((i,j), flips) = ai_fun !gs in (* get the AI move and pieces to flip *)
    paint_button (i,j) color; (* paint the AI's move *)
    flip_buttons flips color; (* flip the necessary pieces based on AI's move *)
    gs := (make_move !gs (i,j) flips); (* update the game state *)
    (* if the human has no moves after the AI goes, the AI must move again until
     * the human has moves or the AI has no moves, in which case the game is over *)
    let cp = get_current_player () in
    let cb = (!gs).board in
    match get_possible_moves cb cp with
    | [] ->
      switch_player ();
      let cp = get_current_player () in
      let cb = (!gs).board in
      gs := ({current_player = cp; board = cb});
      print_endline "AI moved more than once in a row because you had no moves";
      make_ai_move gs color ai_fun; ()
    | _ -> ()
  else  ()

(** If the game is over [end_game_func] changes the turn notifier button's
 * image into a game-over image displaying who won or that there was a draw.
 * Takes in turn game_state [gs], the current player [cp], the next player [np],
 * and the turn notifier button [tnb] *)
let end_game_func gs cp np tnb : unit =
  match (is_game_over !gs cp np) with
  | true when (winning_player !gs) = Black ->
    tnb#remove (tnb#child);
    xpm_label_box ~file:"Blackwon.xpm" ~packing:tnb#add ()
  | true when (winning_player !gs) = White ->
    tnb#remove (tnb#child);
    xpm_label_box ~file:"Whitewon.xpm" ~packing:tnb#add ()
  | true when (winning_player !gs) = Empty ->
    tnb#remove (tnb#child); xpm_label_box ~file:"gries_draw.xpm" ~packing:tnb#add ()
  | false -> ()
  | _ -> ()

(** [gui_human] executes the human's move and updates the gui appropriately.
 * Takes in the game_state [gs], the table button [tbb], the turn notifier
 * button [tnb], a color [color], the coordinate of the button [(x,y)],
 * and a list of pieces to flip [to_flip] *)
let gui_human gs tbb tnb color (x,y) to_flip : unit =
  (* checks to see if game is over before moving *)
  let cp = (get_current_player ()) in
  let np = (get_next_player ()) in
  end_game_func gs cp np tnb;
  (* update the gui to paint the button the user clicked on *)
  (match color with
  | Black ->
    xpm_label_box ~file:(!p1_image) ~packing:tbb#add ();
    flip_buttons (to_flip) Black
  | White ->
    xpm_label_box ~file:(!p2_image) ~packing:tbb#add ();
    flip_buttons (to_flip) White
  | _ -> ());
  (* update the game_state, [make_move] changes the turn to the next player *)
  gs := (make_move !gs (x,y) to_flip);
  (* checks if game over after player moved *)
  let cp = (get_current_player ()) in
  let np = (get_next_player ()) in
  end_game_func gs cp np tnb;
  (* current player should be AI or other human player at this point.
   * Check to see if current player has moves,
   * if it doesn't then set the turn to next player
   * This is necessary for a player to be able to move multiple
   * times in a row when their opponent has no moves. *)
  (match get_possible_moves ((!gs).board) cp with
  | [] ->
    switch_player ();
    gs := {current_player = get_current_player ();
          board = (!gs).board}; ()
  | _ ->
    (* AI or other human player had moves, change the turn button on the gui,
     * proceed to AI function/other human player's turn *)
    tnb#remove (tnb#child);
    if color = Black then
    xpm_label_box ~file:"whitesturn.xpm" ~packing:tnb#add ()
    else xpm_label_box ~file:"blacksturn.xpm" ~packing:tnb#add ())

(** [gui_ai] executes the AI's move and updates the gui appropriately
 * Takes in game_state [gs], turn notifier button [tnb],
 * and GWindow [w] *)
let gui_ai gs tnb w : unit =
  (* pause and display the updated gui
   * so the user sees it's move before the AI moves *)
  Unix.sleepf (0.5);
  w#show;
  (* checks to see if game is over before moving *)
  let cp = (get_current_player ()) in
  let np = (get_next_player ()) in
  end_game_func gs cp np tnb;
  (try (
    (* if the current player is the AI, then change turn notifier
     * [turn_button] to the human's turn, change in gui
     * won't be displayed till after the AI moves *)
    (match cp.ptype, cp.tok with
    | AI, White ->
      tnb#remove (tnb#child);
      xpm_label_box ~file:"blacksturn.xpm" ~packing:tnb#add ()
    | AI, Black ->
      tnb#remove (tnb#child);
      xpm_label_box ~file:"whitesturn.xpm" ~packing:tnb#add ()
    | _ -> ());
    (* make the AI's move, update the gamestate:
       if the current player is not the AI [make_ai_move]
       will have no effect *)
    make_ai_move gs cp.tok !ai_algorithm;)
  with
  | NoMove -> () (* catch exception if AI has no moves,
                    allows user to move again *)
  | _ -> print_endline "failure from AI function");
  (* checks if game over after AI moved *)
  let cp = (get_current_player ()) in
  let np = (get_next_player ()) in
  end_game_func gs cp np tnb

(* initialize the GUI Main loop *)
let locale = GtkMain.Main.init ()

(* ------------- CREATES THE GUI AND RUNS THE GAME --------------*)
let create_gui () : unit =
  (* initialize back end game board representation *)
  let gs_ref : game_state ref =
    ref {current_player = get_current_player (); board = init_board ()} in
  (* window on which the GUI is made *)
  let window = GWindow.window ~width:700 ~height:700
                              ~title:"Othello" () in
  (* vbox necessary for packing GUI and turn button into window *)
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:GMain.quit;
  (* displays whose turn it is *)
  let turn_button = GButton.button ~packing:vbox#add () in
  xpm_label_box ~file:"blacksturn.xpm" ~packing:turn_button#add ();
  (* Table *)
  let hbox_table = GPack.hbox ~packing:vbox#add () in
  let table = GPack.table ~homogeneous:true ~width: 475 ~height: 475
    ~packing:hbox_table#add () in
  (* Creates Grid of button listeners *)
  for x = 0 to !size - 1 do
    for y = 0 to !size - 1 do
      let button = GButton.button () in
      Hashtbl.add button_table (x,y) button;
      table#attach ~left:x ~top:y ~expand:`BOTH (button#coerce);
      (* initialize Othello game board with four pieces in the middle *)
      (if (x,y) = (((!size - 2)/2), ((!size - 2)/2))
        then xpm_label_box ~file:(!p1_image) ~packing:button#add ()
      else if (x,y) = (((!size - 2)/2), ((!size - 2)/2 + 1))
        then  xpm_label_box ~file:(!p2_image) ~packing:button#add ()
      else if (x,y) = (((!size - 2)/2 + 1), ((!size - 2)/2))
        then xpm_label_box ~file:(!p2_image) ~packing:button#add ()
      else if (x,y) = (((!size - 2)/2 + 1), ((!size - 2)/2 + 1))
        then xpm_label_box ~file:(!p1_image) ~packing:button#add ()
      else ());
     (* called when user presses button on gui *)
     button#connect#pressed ~callback: (fun () ->
        (* gets all possible moves *)
        let cp = (get_current_player ()) in
        let cb = ((!gs_ref).board) in
        let poss_moves = get_possible_moves cb cp in
        (* gets list of pieces to flip if (x,y) is valid move *)
        let to_flip = get_flips (x,y) poss_moves in
        (* checks if a piece can be placed on this button *)
        match (is_filled button) with
        | true -> ()
        | false when (get_current_player ()).tok = Black && to_flip <> [] ->
            gui_human gs_ref button turn_button Black (x,y) to_flip
        | false when (get_current_player ()).tok = White && to_flip <> [] ->
            gui_human gs_ref button turn_button White (x,y) to_flip
        | _ -> ());
      (match (snd (current_settings.players)) with (* called when user releases button on gui *)
      | "AI" -> button#connect#released ~callback: (fun () ->
            gui_ai gs_ref turn_button window); ()
      | _ -> () (*not playing against an AI *))
    done
  done;

(* If the user is playing against the AI and the AI is selected to move first,
 * it's first move must be hardcoded becaue all subsequent AI moves are called
 * by the user making a move *)
ai_first_move (get_current_player ()) gs_ref !ai_algorithm turn_button;

(* Display the windows *)
window#show ();
GMain.main () (* runs the GUI main loop *)

