open Settings

(** [create_gui] creates the GUI representation of the game
 * on which the user can play Othello *)
val create_gui: unit -> unit

(** Create a new hbox with an image packed into it
 * and pack the box. Taken from LablGtk tutorial:
 * http://tinyurl.com/zskf6jt *)
val xpm_label_box: file:string -> packing:(GObj.widget -> unit) -> unit -> unit

(** Initialize the images that represent player1 and player2 on the GUI given
 * the current settings [cs] *)
val initialize_images: settings -> unit