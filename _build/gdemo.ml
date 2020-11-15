(** Simple test case for some of the graphics context functions. *)

module Graphics = G

(* You do not need to modify this file. Instead, Task 0 requires you to    
   implement functionality in gctx.ml to make the window that this file    
   displays look like the one on the assignment website.                   *)

let run () : unit =

  (* Open the graphics window and create a top-level graphics context. *)
  Gctx.open_graphics ();
  let gc = Gctx.top_level in

  (* Draw a line *)
  Gctx.draw_line gc (10, 10) (100, 100);

  (* Draw a triangle *)
  let p1 = (20,70) in
  let p2 = (40,110) in
  let p3 = (70,90) in

  Gctx.draw_line gc p1 p2;
  Gctx.draw_line gc p2 p3;
  Gctx.draw_line gc p3 p1;

  (* Draw text *)
  Gctx.draw_string gc (20,150) "CIS 120";

  (* Draw a red square. NOTE: this won't do anything 
     until you update the draw_rect function in Gctx. *)
  let red_gc  = Gctx.with_color gc Gctx.red in   (* Not Graphics.red! *)
  Gctx.draw_rect red_gc (50,10) (35,35);

  (* Draw a blue ellipse. NOTE: this won't do anything 
     until you update the draw_ellipse function in Gctx. *)
  let blue_gc = Gctx.with_color gc Gctx.blue in
  Gctx.draw_ellipse blue_gc (42,42) 12 12;

  (* Tell the graphics window to do the thing! *)
  Graphics.synchronize ();

  (* loop until a key is pressed, so we can see the window. *)
  try 
    Graphics.loop [Graphics.Key_pressed] (fun (s:Graphics.status) -> exit 0)
  with Graphics.Graphic_failure s ->
    (print_endline "Program exited"; exit 0)

;; run ()
