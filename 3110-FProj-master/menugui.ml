open Player
open GMain
open Board
open Settings
open Gamegui
open Ai

(* callbacks for the gui*)
let delete_event ev =
	GMain.Main.quit ();
 	false

(** create the start menu, from which the user can select to start the game
 * or edit the game default options *)
let create_menu_gui () =
  	let window = GWindow.window ~title:"Settings" ~border_width:10 ~width:700
  		~height:700 () in
	(* Here we just set a handler for delete_event that immediately
 	 * exits GTK. *)
	window#event#connect#delete ~callback:delete_event;
	let main = GPack.vbox ~packing:window#add () in
	let options = GPack.hbox ~packing:main#add ~height:5 () in
	let play = GPack.hbox ~packing:main#add ~border_width:100 () in
	(*who_goes_first*)
	let vbox1 = GPack.vbox ~packing:options#add () in
  let label = GMisc.label ~text: "Who goes First" ~use_underline: true
    ~packing: vbox1#add () in
	let button1w = GButton.radio_button ~label:"Human" ~packing:vbox1#add
	   	~active:true () in
	let button2w = GButton.radio_button
    	~group:button1w#group ~label:"AI" ~packing:vbox1#add () in
  button1w#connect#clicked 
    ~callback: (fun () -> ignore (change_who_goes "human"); ());
  button2w#connect#clicked 
    ~callback: (fun () -> ignore (change_who_goes "AI"); ());
  (*who_vs_whom*)
  let vbox2 = GPack.vbox ~packing:options#add () in
  let label = GMisc.label ~text: "Game Mode" ~use_underline: true 
    ~packing: vbox2#add () in
  let button1g = GButton.radio_button ~label:"Player vs Player" 
    ~packing:vbox2#add () in
	let button2g = GButton.radio_button ~group:button1g#group 
    ~label:"Player vs AI" ~active:true ~packing:vbox2#add () in
  button1g#connect#clicked 
    ~callback: (fun () -> ignore (change_players("human","human")); ());
  button2g#connect#clicked 
    ~callback: (fun () -> ignore (change_players("human","AI")); ());
  (*difficulty*)
 	let vbox3 = GPack.vbox ~packing:options#add () in
  let label = GMisc.label ~text: "Difficulty" ~use_underline: true 
    ~packing: vbox3#add () in
 	let button1d = GButton.radio_button ~label:"Easy" ~packing:vbox3#add () in
	let button2d = GButton.radio_button ~group:button1d#group ~label:"Normal"
      ~active:true ~packing:vbox3#add () in
	let button3d = GButton.radio_button
    	~group:button1d#group ~label:"Hard" ~packing:vbox3#add () in
  let button4d = GButton.radio_button
    	~group:button1d#group ~label:"Legendary" ~packing:vbox3#add () in
  button1d#connect#clicked 
    ~callback: (fun () -> ignore (change_difficulty(0)); ());
  button2d#connect#clicked 
    ~callback: (fun () -> ignore (change_difficulty(1)); ());
  button3d#connect#clicked 
    ~callback: (fun () -> ignore (change_difficulty(2)); ());
  button4d#connect#clicked 
    ~callback: (fun () -> ignore (change_difficulty(3)); ());
  (*board_size*)
  let vbox4 = GPack.vbox ~packing:options#add () in
  let label = GMisc.label ~text: "Board Size" ~use_underline: true 
    ~packing: vbox4#add () in
  let button1s = GButton.radio_button ~label:"8 X 8" ~packing:vbox4#add
      ~active:true () in
  let button2s = GButton.radio_button
      ~group:button1s#group ~label:"6 X 6" ~packing:vbox4#add () in
  let button3s = GButton.radio_button
      ~group:button1s#group ~label:"4 X 4" ~packing:vbox4#add () in
  button1s#connect#clicked ~callback:(fun () ->ignore (change_board_size 8);());
  button2s#connect#clicked ~callback:(fun () ->ignore (change_board_size 6);());
  button3s#connect#clicked ~callback:(fun () ->ignore (change_board_size 4);());
  (*piece images *)
  let vbox5 = GPack.vbox ~packing:options#add () in
  let label = GMisc.label ~text: "Images for Players" ~use_underline: true 
    ~packing: vbox5#add () in
  let button1i = GButton.radio_button ~label:"Black vs White" ~packing:vbox5#add
    ~active:true () in
  let button2i = GButton.radio_button
  ~group:button1i#group ~label:"Clarkson vs Darth Ref" ~packing:vbox5#add () in
  let button3i = GButton.radio_button
      ~group:button1i#group ~label:"Fire vs Water" ~packing:vbox5#add () in
  button1i#connect#clicked 
  ~callback:(fun ()->ignore (change_image ("b_circle.xpm", "w_circle.xpm"));());
  button2i#connect#clicked 
  ~callback: (fun () -> ignore (change_image ("clarkson.xpm", "vader.xpm"));());
  button3i#connect#clicked 
  ~callback: (fun () -> ignore (change_image ("fire.xpm", "water.xpm")); ());
  let buttonf = GButton.button ~packing:play#add () in
  xpm_label_box ~file:"startbutton.xpm" ~packing:buttonf#add ();
  buttonf#connect#clicked ~callback: (fun () ->
    (initialize_players current_settings);
    (initialize_difficulty current_settings);
    (initialize_size current_settings);
    (initialize_images current_settings);
    create_gui (); ());
  (* Display the window. *)
	window#show ();
	(* Rest in GMain.Main.main and wait for the fun to begin! *)
	GMain.Main.main ()

let _  = create_menu_gui ()
