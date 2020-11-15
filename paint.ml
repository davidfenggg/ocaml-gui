(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of { color: color; p1: point; p2: point }
  | Points of { color: color; points: point list }
  | Ellipse of { color: color; p1: point; p2: point }
  | ThickLine of { color: color; p1: point; p2: point }
  | ThickEllipse of { color: color; p1: point; p2: point }

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location of
        the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 2, 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseMode

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;
  
  (* The previewed shape on the canvas as the mouse is dragged *)
  mutable preview : shape option; 

  (* Whether a thick line/ellipse should be drawn *)
  mutable thick : bool;

  (* TODO: You will need to add new state for Tasks 2, 3, 5, and *)
  (* possibly 6 *) 
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thick = false;
  (* TODO: You will need to add new state for Tasks 2, 3, 5, and maybe 6 *)
  
}



(** This function creates a graphics context with the appropriate
    pen color and thickness.
*)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (b: bool) : gctx =
  let g1 = with_color g c in
  let g2 = with_thickness g1 b in
  g2


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview.  *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color false) l.p1 l.p2
      | Points ps -> draw_points (with_params g ps.color false) ps.points
      | Ellipse el ->
        let (x1, y1) = el.p1 in
        let (x2, y2) = el.p2 in
        let x_center = (x1 + x2) / 2 in
        let y_center = (y1 + y2) / 2 in
        draw_ellipse (with_params g el.color false) (x_center, y_center) 
        (abs (x_center - x1)) (abs (y_center - y1))
      | ThickLine l -> draw_line (with_params g l.color true) l.p1 l.p2
      | ThickEllipse el -> 
        let (x1, y1) = el.p1 in
        let (x2, y2) = el.p2 in
        let x_center = (x1 + x2) / 2 in
        let y_center = (y1 + y2) / 2 in
        draw_ellipse (with_params g el.color true) (x_center, y_center) 
        (abs (x_center - x1)) (abs (y_center - y1))
    end in
  Deque.iterate draw_shape paint.shapes;
  begin match paint.preview with
  | None -> ()
  | Some x -> draw_shape x
  end

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur 
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p : position = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
         | LineStartMode ->
           (* The paint_canvas was waiting for the first click of a line,   
              so change it to LineEndMode, recording the starting point of  
              the line. *)
           paint.mode <- LineEndMode p
         | LineEndMode p1 ->
         (* The paint_canvas was waiting for the second click of a line,  
            so create the line and add it to the deque of shapes. Go back 
            to waiting for the first click. *)
         Deque.insert_tail (Line {color=paint.color; p1=p1; p2=p}) paint.shapes;
                      paint.mode <- LineStartMode
         (* The canvas gets ready to draw a point, which becomes the 
         previewed shape *)
         | PointMode -> 
         paint.preview <- Some (Points {color=paint.color; points=[p]})
         (* The canvas gets ready to draw an ellipse by setting the previewd
         shape as the ellipse; it could be thin or thick *)
         |EllipseMode -> 
         if paint.thick then 
         paint.preview <- Some (ThickEllipse {color=paint.color; p1=p; p2=p})
         else 
         paint.preview <- Some (Ellipse {color=paint.color; p1=p; p2=p})
       end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged    
         with the button down. Initially there is nothing to do, but you'll  
         need to update this part for Task 2, 3, 4 and maybe 6. *)
      (begin match paint.mode with
      | LineStartMode -> ()
      | LineEndMode p1 -> 
      if paint.thick
      then paint.preview <- Some (ThickLine {color=paint.color; p1=p1; p2=p})
      else
      paint.preview <- Some (Line {color=paint.color; p1=p1; p2=p})
      | PointMode -> 
      let points =  
        begin match paint.preview with  
        | Some (Points ps) -> ps.points  
        | _ -> []  
        end in
      let new_points = points @ [p] in
      paint.preview <- Some (Points {color=paint.color; points=new_points})
      |EllipseMode -> 
      let old_pt =
        begin match paint.preview with
        | Some (Ellipse el) -> el.p1
        | Some (ThickEllipse el) -> el.p1
        | _ -> (0,0)
        end in
      if paint.thick 
      then
      paint.preview <- Some (ThickEllipse {color=paint.color; p1=old_pt; p2=p})
      else
      paint.preview <- Some (Ellipse {color=paint.color; p1=old_pt; p2=p})
      end
      )
      
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2, *)
      (* 3, 4, and possibly 6 need to do something different here.           *)
      (
      begin match paint.mode with
        | LineStartMode -> failwith "paint mode should have been changed"
        | LineEndMode p1 -> 
        paint.preview <- None;
        if paint.thick then
        (Deque.insert_tail (ThickLine {color=paint.color; p1=p1; p2=p}) 
        paint.shapes; paint.mode <- LineStartMode;)
        else 
        (Deque.insert_tail (Line {color=paint.color; p1=p1; p2=p}) paint.shapes;
        paint.mode <- LineStartMode;)
        | PointMode -> 
        let pts =  
          begin match paint.preview with  
          | Some (Points ps) -> ps.points  
          | _ -> []  
        end in
        paint.preview <- None;
        Deque.insert_tail (Points {color=paint.color; points=pts}) paint.shapes;
        | EllipseMode ->
        let (pt1, pt2) =
          begin match paint.preview with
          | Some (Ellipse el) -> (el.p1, el.p2)
          | Some (ThickEllipse el) -> (el.p1, el.p2)
          | _ -> ((0,0), (0,0))
          end in
        paint.preview <- None;
        if paint.thick then
        (Deque.insert_tail (ThickEllipse {color=paint.color; p1=pt1; p2=pt2}) 
        paint.shapes;)
        else 
        (Deque.insert_tail (Ellipse {color=paint.color; p1=pt1; p2=pt2}) 
        paint.shapes;)
      end
        )
    
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over  
       the canvas without pushing any buttons) and the KeyPress event (where 
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** BARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the
   paint program -- the buttons, color selectors, etc., and
   lays them out in the top - level window. *)
(* TODO: Tasks 1, 2, 4, 5, and 6 involving adding new buttons or
   changing the layout of the Paint GUI. Initially the layout is very
   ugly because we use only the hpair widget demonstrated in
   Lecture. Task 1 is to make improvements to make the layout more
   appealing. You may choose to arrange the buttons and other GUI
   elements of the paint program however you like (so long as it is
   easily apparent how to use the interface ).  The sample screen shot
   of our solution provides one possible design.  Also, feel free to
   improve the visual components of the GUI, for example, our solution
   puts borders around the buttons and uses a custom "color button"
   that changes its appearance based on whether or not the color is
   currently selected.  *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
   It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 2, 3 and 4. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** A spacer widget *)
let spacer : widget = space (10,10)

(* Create the Line, Ellipse, and Point Buttons *)
let (w_point, lc_point, nc_point) = button "Point"
let (w_line, lc_line, nc_line) = button "Line"
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

let change_line_mode () : unit =
  paint.mode <- LineStartMode;

;;nc_line.add_event_listener (mouseclick_listener change_line_mode)

let change_point_mode () : unit =
  paint.mode <- PointMode;
  
;;nc_point.add_event_listener (mouseclick_listener change_point_mode)

let change_ellipse_mode () : unit =
  paint.mode <- EllipseMode;
  
;;nc_ellipse.add_event_listener (mouseclick_listener change_ellipse_mode)

(* Adds the checkbox for thickness *)
let (w_thick, vc_thick) = checkbox false "Thick"

;;vc_thick.add_change_listener (fun x -> paint.thick <- not paint.thick)

(* Adds the sliders for the RGB colors *)
let (s_red, vc_red) = slider {r=255;g=0;b=0} (Gctx.get_red (paint.color)) 256

let (s_green, vc_green) = slider {r=0;g=255;b=0} (Gctx.get_green (paint.color)) 
256

let (s_blue, vc_blue) = slider {r=0;g=0;b=255} (Gctx.get_blue (paint.color)) 256

;;vc_red.add_change_listener (fun x ->
let c = {r = vc_red.get_value(); g = paint.color.g; b = paint.color.b } in
paint.color <- c
)

;;vc_green.add_change_listener (fun x ->
let c = {r = paint.color.r; g = vc_green.get_value(); b = paint.color.b } in
paint.color <- c
)

;;vc_blue.add_change_listener (fun x ->
let c = {r = paint.color.r; g = paint.color.g; b = vc_blue.get_value() } in
paint.color <- c
)

(** The mode toolbar, initially containing just the Undo button. *)
(*  TODO: you will need to add more buttons to the toolbar in 
    Tasks 3, 5, and possibly 6. *)
let mode_toolbar : widget = hlist[spacer; 
border w_point; spacer; border w_line; spacer; border w_ellipse; 
spacer; border w_thick; spacer; border w_undo]

(* Creates the set of color sliders *)
let slider_toolbar : widget = vlist [s_red; spacer; s_green; spacer; 
s_blue]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  nc.add_event_listener (mouseclick_listener (fun () -> 
      vc_red.change_value (Gctx.get_red c)));
  nc.add_event_listener (mouseclick_listener (fun () -> 
      vc_green.change_value (Gctx.get_green c)));
  nc.add_event_listener (mouseclick_listener (fun () -> 
      vc_blue.change_value (Gctx.get_blue c)));
  w

(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
   let color_toolbar : widget =   
   hlist [color_indicator; spacer; color_button black; spacer; 
   color_button white; spacer; color_button red; spacer; color_button green; 
   spacer; color_button blue; spacer; color_button yellow; spacer; 
   color_button cyan; spacer; color_button magenta]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
   
let right_widget = vlist[mode_toolbar; spacer; color_toolbar]

let paint_widget =
  vlist [paint_canvas; spacer; hlist[slider_toolbar; spacer; right_widget]]

(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
