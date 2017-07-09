open OUnit2
open Game
open Board
open Player

let p1 = ref {ptype = Human; tok = Black; turn = First}
let p2 = ref {ptype = AI; tok = White; turn = Second}
let b1 = init_board ()
let b2 = let empty_board = Array.make_matrix 8 8 Empty in
  empty_board.(3).(3) <- Black;
  empty_board.(3).(4) <- Black;
  empty_board.(3).(5) <- Black;
  empty_board.(4).(4) <- Black;
  empty_board.(4).(3) <- White;
  empty_board
let b3 = let empty_board = Array.make_matrix 8 8 Empty in
  empty_board.(3).(4) <- Black;
  empty_board.(3).(5) <- Black;
  empty_board.(4).(4) <- Black;
  empty_board.(2).(3) <- White;
  empty_board.(3).(3) <- White;
  empty_board.(4).(3) <- White;
  empty_board
let b4 = let empty_board = Array.make_matrix 8 8 Black in
  empty_board
let b5 = let empty_board = Array.make_matrix 8 8 White in
  empty_board.(3).(4) <- Empty;
  empty_board
let gs1 = {current_player = !p1; board = b1}
let gs2 = {current_player = !p2; board = b2}
let gs3 = {current_player = !p1; board = b3}
let gs4 = {current_player = !p2; board = b4}
let gs5 = {current_player = !p2; board = b5}
let x = set_p1 !p2
let x = set_p2 !p1
let x = set_current_player !p1

let board_tests =
[
  "quick_is_valid_move test 1" >:: 
    (fun _ -> assert_equal true (quick_is_valid_move b1 (2,4) (!p1)));
  "quick_is_valid_move test 2" >:: 
    (fun _ -> assert_equal false (quick_is_valid_move b1 (5,4) (!p1)));
  "quick_is_valid_move test 3" >:: 
    (fun _ -> assert_equal true (quick_is_valid_move b1 (4,2) (!p1)));
  "quick_is_valid_move test 4" >:: 
    (fun _ -> assert_equal false (quick_is_valid_move b1 (3,4) (!p1)));
  "quick_is_valid_move test 5" >:: 
    (fun _ -> assert_equal false (quick_is_valid_move b1 (2,3) (!p1)));
  "quick_is_valid_move test 6" >:: 
    (fun _ -> assert_equal false (quick_is_valid_move b1 (3,2) (!p1)));
  "is_valid_move test 1" >:: 
  	(fun _ -> assert_equal [(3, 4)] (is_valid_move b1 (2,4) (!p1)));
  "is_valid_move test 2" >:: 
  	(fun _ -> assert_equal [] (is_valid_move b1 (5,4) (!p1)));
  "is_valid_move test 3" >:: 
  	(fun _ -> assert_equal [(4, 3)] (is_valid_move b1 (4,2) (!p1)));
  "is_valid_move test 4" >:: 
  	(fun _ -> assert_equal [] (is_valid_move b1 (3,4) (!p1)));
  "is_valid_move test 5" >:: 
  	(fun _ -> assert_equal [] (is_valid_move b1 (2,3) (!p1)));
  "is_valid_move test 6" >:: 
  	(fun _ -> assert_equal [] (is_valid_move b1 (3,2) (!p1)));
  "get_possible_moves test 1" >:: 
    (fun _ -> assert_equal [((5, 3), [(4, 3)]); ((4, 2), [(4, 3)]); 
      ((3, 5), [(3, 4)]); ((2, 4), [(3, 4)])] (get_possible_moves b1 (!p1)));
  "get_possible_moves test 2" >:: 
    (fun _ -> assert_equal [((5, 4), [(4, 4)]); ((4, 5), [(4, 4)]); 
      ((3, 2), [(3, 3)]); ((2, 3), [(3, 3)])] (get_possible_moves b1 (!p2)));
  "get_possible_moves test 3" >:: 
    (fun _ -> assert_equal 
    	[((5, 3), [(4, 3)]); ((5, 2), [(4, 3)]); ((4, 2), [(4, 3)])] 
        (get_possible_moves b2 (!p1)));
  "get_possible_moves test 4" >:: 
    (fun _ -> assert_equal 
    	[((4, 5), [(4, 4)]); ((2, 5), [(3, 4)]); ((2, 3), [(3, 3)])]
        (get_possible_moves b2 (!p2)));
  "get_possible_moves test 5" >:: 
    (fun _ -> assert_equal [((5, 2), [(4, 3)]); ((4, 2), [(4, 3)]); ((3, 2), 
    	[(3, 3)]); ((2, 2), [(3, 3)]); ((1, 2), [(2, 3)])]  
    	(get_possible_moves b3 (!p1)));
  "get_possible_moves test 6" >:: 
    (fun _ -> assert_equal [((5, 5), [(4, 4)]); ((4, 5), [(4, 4); (3, 4)]); 
    	((3, 6), [(3, 5); (3, 4)]); ((2, 5), [(3, 4)])] 
    	(get_possible_moves b3 (!p2)));
  "get_possible_moves test 7" >:: 
    (fun _ -> assert_equal [] (get_possible_moves b4 (!p1)));
  "get_possible_moves test 8" >:: 
    (fun _ -> assert_equal [] (get_possible_moves b4 (!p2)));
  "is_board_full test 1" >:: 
    (fun _ -> assert_equal false (is_board_full b1));
  "is_board_full test 2" >:: 
    (fun _ -> assert_equal false (is_board_full b2));
  "is_board_full test 3" >:: 
    (fun _ -> assert_equal false (is_board_full b3));
  "is_board_full test 4" >:: 
    (fun _ -> assert_equal true (is_board_full b4));
  "is_board_full test 4" >:: 
    (fun _ -> assert_equal false (is_board_full b5));
  "is_frontier test 1" >:: 
    (fun _ -> assert_equal false (is_frontier b4 (2,3)));
  "is_frontier test 2" >:: 
    (fun _ -> assert_equal false (is_frontier b4 (2,2)));
  "is_frontier test 3" >:: 
    (fun _ -> assert_equal false (is_frontier b4 (3,3)));
  "is_frontier test 4" >:: 
    (fun _ -> assert_equal false (is_frontier b4 (3,4)));
  "is_frontier test 5" >:: 
    (fun _ -> assert_equal false (is_frontier b4 (8,8)));
  "is_frontier test 6" >:: 
    (fun _ -> assert_equal true (is_frontier b2 (3,6)));
  "is_frontier test 7" >:: 
    (fun _ -> assert_equal true (is_frontier b2 (4,6)));
  "is_frontier test 8" >:: 
    (fun _ -> assert_equal true (is_frontier b2 (7,7)));
  "is_frontier test 9" >:: 
    (fun _ -> assert_equal true (is_frontier b2 (3,3)));
]

let game_tests =
[
  "is_game_over test 1" >:: 
    (fun _ -> assert_equal false (is_game_over gs1 !p1 !p2));
  "is_game_over test 2" >:: 
    (fun _ -> assert_equal false (is_game_over gs2 !p2 !p1));
  "is_game_over test 3" >:: 
    (fun _ -> assert_equal false (is_game_over gs3 !p1 !p2));
  "is_game_over test 4" >:: 
    (fun _ -> assert_equal true (is_game_over gs4 !p1 !p2));
  "is_game_over test 5" >:: 
    (fun _ -> assert_equal true (is_game_over gs5 !p1 !p2));
  "winning_player test 1" >:: 
    (fun _ -> assert_equal Empty (winning_player gs1));
  "winning_player test 2" >:: 
    (fun _ -> assert_equal Black (winning_player gs2));
  "winning_player test 3" >:: 
    (fun _ -> assert_equal Empty (winning_player gs3));
  "winning_player test 4" >:: 
    (fun _ -> assert_equal Black (winning_player gs4));
  "winning_player test 5" >:: 
    (fun _ -> assert_equal White (winning_player gs5));
]

let player_tests = [
  "get_current_player test 1" >:: 
  	(fun _ -> assert_equal !p1 (get_current_player ()));
  "get_next_player test 1" >:: 
  	(fun _ -> assert_equal !p2 (get_next_player ()));
  "extract_from_option test 1" >:: 
  	(fun _ -> assert_equal !p1 (extract_from_option (Some !p1)));
  "extract_from_option test 2" >:: 
  	(fun _ -> assert_equal !p2 (extract_from_option (Some !p2)));
  "other_tok test 1" >:: 
  	(fun _ -> assert_equal White (other_tok Black));
  "other_tok test 2" >:: 
  	(fun _ -> assert_equal Black (other_tok White));
]

let suite = "test suite" >::: 
  board_tests @ game_tests @ player_tests
 
let _ = run_test_tt_main suite