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
let player1 = ref (Some {ptype = Human; tok = Black; turn = First})
let player2 = ref (Some {ptype = AI; tok = White; turn = Second})

(* [set_p1 p] sets player1's value to player [p]. *)
let set_p1 p =
  player1 := Some p

(* [set_p2 p] sets player2's value to player [p]. *)
let set_p2 p =
  player2 := Some p

(* Reference to the current player. *)
let current_player_ref = ref (!player1)

(* [initialize_players cs] takes in the current settings from settings [cs] and
 * initializes the players. *)
let initialize_players cs =
  match cs.who_goes, cs.players with
  | "human", ("human","human") ->
    set_p1 {ptype = Human; tok = Black; turn = First};
    set_p2 {ptype = Human; tok = White; turn = Second};
    (current_player_ref := Some {ptype = Human; tok = Black; turn = First}); ()
  | "AI", ("human","AI") ->
    set_p1 {ptype = AI; tok = Black; turn = First};
    set_p2 {ptype = Human; tok = White; turn = Second};
    (current_player_ref := Some {ptype = AI; tok = Black; turn = First}); ()
  | "human", ("human","AI") ->
    set_p1 {ptype = Human; tok = Black; turn = First};
    set_p2 {ptype = AI; tok = White; turn = Second};
    (current_player_ref := Some {ptype = Human; tok = Black; turn = First}); ()
  | _ -> failwith "you did not select a valid settings configuration"

(* [set_current_player p] sets the current player's value to a player [p]. *)
let set_current_player p = current_player_ref := Some p

(* [extract_from_option p] extracts the value of a player option. Failwith
 * "next_player called when none made" when the player option has None. *)
let extract_from_option p =
  match p with
  | Some x -> x
  | None -> failwith "next_player called when none made"

(* [get_current_player ()] returns the current player who's playing the game. *)
let get_current_player () =
  extract_from_option (!current_player_ref)

(* [get_next_player ()] returns the next player who's playing the game. *)
let get_next_player () =
  match !current_player_ref with
  | Some x -> if x = extract_from_option (!player1)
      then extract_from_option (!player2) else extract_from_option (!player1)
  | None -> failwith "next_player called when none made"

(* [switch_player ()] sets current_player_ref equal to the next_player. *)
let switch_player () =
  let p1 = extract_from_option (!player1) in
  let p2 = extract_from_option (!player2) in
  let cp = extract_from_option (!current_player_ref) in
  if cp = p1 then (current_player_ref := (!player2))
  else if cp = p2 then (current_player_ref := (!player1))
  else failwith "current player is not a valid player"

(* [other_tok] returns the other token of the current token. Failwith
 * "There's no other token" if the input token is Empty. *)
let other_tok = function
  | Black -> White
  | White -> Black
  | Empty -> failwith "There's no other token"

