(** The main eventloop of the GUI library *)

(* Do not modify this file. *)


(**
   This function takes a widget, which is the "root" of the GUI interface.
   It creates the "top-level" Gctx, and then it goes into an infinite loopx.
   The loop simply repeats these steps forever:
     - clear the graphics window
     - ask the widget to repaint itself
     - wait for a user input event
     - forward the event to the widget's event handler
*)
val run : Widget.widget -> unit
