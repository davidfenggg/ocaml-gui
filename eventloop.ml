(** The main eventloop of the GUI library *)

(* Do not modify this file. *)

module Graphics = G
;; open Gctx
;; open Widget

let string_of_status (s: Graphics.status) : string =
  "(" ^ (string_of_int s.Graphics.mouse_x) ^ ","
  ^ (string_of_int s.Graphics.mouse_y) ^ ") b:"
  ^ (string_of_bool s.Graphics.button)
  ^ (if s.Graphics.keypressed
     then "k:" ^ (String.make 1 s.Graphics.key)
     else "")

(* This function is used by the eventloop to get the next event from the
   graphics library. The events reported by the library are not as informative
   as we would like, so this function also processes them into a more convenient
   form.  You don't need to understand how this processing works, but you do
   need to understand the various forms of events that this function generates.

   See gctx.ml for more information about events.
*)

let event_of_status : Graphics.status -> Gctx.event option =

  let initial_status : Graphics.status =
    { Graphics.mouse_x =0;
      Graphics.mouse_y =0;
      Graphics.key = ' ';
      Graphics.button = false;
      Graphics.keypressed = false } in

  let last_status = ref initial_status in

  let compute_rich_event
      (s0 : Graphics.status)
      (s1 : Graphics.status) : event_type option =

    if s1.Graphics.keypressed then Some (KeyPress s1.Graphics.key)
    else if s0.Graphics.button <> s1.Graphics.button then
      (* change in button state *)
      begin
        if s1.Graphics.button then Some MouseDown else Some MouseUp
      end
    else if (s0.Graphics.mouse_x <> s1.Graphics.mouse_x ) ||
            (s0.Graphics.mouse_y <> s1.Graphics.mouse_y ) then
      (* mouse motion *)
      begin
        if s1.Graphics.button then Some MouseDrag else Some MouseMove
      end
    else None
  in
  (fun (status : Graphics.status) ->
     let event_type = compute_rich_event last_status.contents status in
     begin match event_type with
       | None -> last_status.contents <- status; None
       | Some ty ->
         begin
           last_status.contents <- status;
           Some (ty, (status.Graphics.mouse_x, status.Graphics.mouse_y))
         end
     end)

(** This function takes a widget, which is the "root" of the GUI interface.
    It creates the "top-level" Gctx, and then it goes into an infinite loop.
    The loop simply repeats these steps forever:
     - wait for a user input event
     - clear the graphics window
     - forward the event to the widget's event handler 
     - ask the widget to repaint itself *)
let run (w: widget) : unit =
  let g = Gctx.top_level in         (* the top-level Gctx *)
  Gctx.open_graphics ();
  w.repaint g;                      (* paint initial contents *)
  if not Graphics.js_mode then Graphics.synchronize (); 

  Graphics.loop                     (* wait for a user input event *)
    [Graphics.Mouse_motion; Graphics.Button_down;
     Graphics.Button_up; Graphics.Key_pressed]
    (fun status ->
       clear_graph ();
       begin match event_of_status status with
         | None -> ()                 (* spurious update: do nothing *)
         | Some e -> w.handle g e     (* let widget handle the event *)
       end;
       w.repaint g;  (* show the window *)
       if not Graphics.js_mode then Graphics.synchronize ()
    ) 
  
