(* Demo of notifier widgets and friends *)

(* In this file, we build up a series of increasingly sophisticated
   approaches to event handling.  The final version corresponds to
   what's provided by widget.ml.

   The "application" we're implementing is the world's simplest
   lightbulb demo: It uses a canvas widget to paint a rectangle that
   can be either black or yellow.  When the user clicks the mouse
   inside the rectangle, it changes color. *)

(* NOTE: There are no tasks to complete in this file!  Understanding
   how notifiers work will help in tasks 5 and 6... *)

(* NOTE: To run this sequence of examples, uncomment the 
       ;; Eventloop.run w
   command below the version you want to try.  (And make sure the rest are
   commented out.)  Then, do "Build Project" and refresh the notifierDemo.html
   page (or launch a fresh from the "Preview" menu).  *)

;; open Widget
;; open Gctx

(* ---------------------------------------------------------- *)
(* First, we define some bits that will be used by all the versions we
   write. *)

(* The current state of the lightbulb (false=black) *)
type bulb_state = {mutable on : bool}
let bulb = {on = false}

(* A function that paints a rectangle of the appropriate color. *)
let paint_bulb (g : gctx) : unit =
  let g_new = with_color g
      (if bulb.on then yellow else black) in
  fill_rect g_new (0, 0) (99, 99)

(* A function that switches the state of the bulb. *)
let switch_bulb () : unit =
  bulb.on <- not bulb.on

(* ---------------------------------------------------------- *)
(* Our first attempt doesn't work very well: it responds to EVERY
   event by switching the bulb's color. *)

(* A simple_canvas widget provides a region of the screen where
   low-level painting operations can be carried out directly and
   events can be responded to. *)
let simple_canvas
      (dim : dimension)
      (f : gctx -> unit)
      (h : event_listener)
    : widget =
  { repaint = f;
    handle = h;
    size = (fun _ -> dim) }

let handle_bulb : event_listener =
  fun (g : gctx) (e : event) : unit ->
    switch_bulb ()

let w = simple_canvas (100,100) paint_bulb handle_bulb

(* ;; Eventloop.run w *)

(* ---------------------------------------------------------- *)
(* We can fix this behavior by paying attention just to MouseDown
   events and ignoring the rest. *)

let bulb_listener : event_listener =
  fun (g : gctx) (e : event) : unit ->
    if event_type e = MouseDown then switch_bulb ()

let w = simple_canvas (100,100) paint_bulb bulb_listener

(* ;; Eventloop.run w *)

(* ---------------------------------------------------------- *)
(* Since this will be a very common situation, we might as well take a
   moment to build a little abstraction for specialized "mouse
   listeners" that ignore non-mouse events. *)

let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g : gctx) (e : event) ->
    if event_type e = MouseDown then action ()

let bulb_listener : event_listener =
  mouseclick_listener switch_bulb

let w = simple_canvas (100,100) paint_bulb bulb_listener

(* ;; Eventloop.run w  *)

(* ---------------------------------------------------------- *)
(* Something that's a little not nice about the last solution is that
   we've made ALL simple_canvas widgets take an event_handler
   parameter; if we continued in this direction, we'd next need to go
   through and add event_handler parameters to ALL our widget
   constructors, even though probably very few of the widgets in
   typical applications are going to want to handle events.

   We can simplify things by separating the event-handling code from
   the canvas-painting functionality; we do this by making a new
   widget constructor that JUST handles notifications. *)

(* A bare_canvas widget provides a region of the screen where
   low-level painting operations can be carried out directly.  It
   doesn't do anything with events. *)
let bare_canvas (dim : dimension) (f : gctx -> unit) : widget =
  { repaint = f;
    handle = (fun _ _ -> ());
    size = (fun _ -> dim) }

(* A simple_notifier widget is a widget "wrapper" that doesn't take
   up any extra screen space -- it simply enriches an existing widget
   with the ability to react to events.  When an event comes in to the
   notifier, it is passed to the event_listener and then passed to the
   child widget. *)
let simple_notifier (w : widget) (h : event_listener) : widget =
  { repaint = w.repaint;
    handle = (fun (g : gctx) (e : event) -> h g e; w.handle g e);
    size = w.size
  }

let w = simple_notifier (bare_canvas (100,100) paint_bulb) bulb_listener

(* ;; Eventloop.run w *)

(* ---------------------------------------------------------- *)
(* For building more complex user interfaces, a little more
   flexibility is often useful. In particular, an event_listener
   associated with one part of a widget hierarchy may need to perform
   actions on the controllers of widgets in a completely different
   part of the widget hierarchy; in such situations, it may be
   convenient to place the notifier in the widget hierarchy at some
   point but wait until later to provide the event_listener that goes
   with it.

   To make this possible, we change the type of the notifier widget so
   that, rather than taking an event_listener as an argument, it
   RETURNS a "notifier controller" object that can be used to add
   listeners later. *)

type notifier_controller = {
  add_event_listener : event_listener -> unit
}

let notifier (w : widget) : widget * notifier_controller =
  let listeners = { contents = [] } in
  { repaint = w.repaint;
    handle =
      (fun (g : gctx) (e : event) ->
         List.iter (fun h -> h g e) listeners.contents;
         w.handle g e);
    size = w.size
  },
  { add_event_listener =
      fun (newl : event_listener) ->
        listeners.contents <- newl :: listeners.contents
  }

let (w,c) = notifier (bare_canvas (100,100) paint_bulb) 
;; c.add_event_listener bulb_listener

(* ;; Eventloop.run w *)

(* ---------------------------------------------------------- *)
(* To see how this extra flexibility might be useful, let's build a
   slightly more complex application program that maintains both a
   canvas and a label whose text should change when the lightbulb
   changes state. Here, we associate the listener with the 
   canvas.  *)


let (w1,c) = notifier (bare_canvas (100,100) paint_bulb) 
let (w2,l) = label "OFF" 
let w = hpair w1 (hpair (space (10,10)) w2)

let bulb_listener : event_listener =
  mouseclick_listener
    (fun () ->
      switch_bulb ();
      l.set_label (if bulb.on then "ON " else "OFF"))


;; c.add_event_listener bulb_listener

(* ;; Eventloop.run w *)

(* ---------------------------------------------------------- *)
(* Buttons at last! *)

(* By slightly refactoring the above code to put the 
   notifier around the label rather than the canvas, we
   have actually arrived at code that works as a button!
   
   A button is just a label (with its associated controller)
   wrapped in a notifier.  We install button actions by 
   adding mouseclick_listeners to the notifier.
*)

let w1 = bare_canvas (100,100) paint_bulb
let (w2,l) = (label "OFF")
let (on_off_button, c) = notifier w2

let bulb_listener : event_listener =
  mouseclick_listener
    (fun () ->
      switch_bulb ();
      l.set_label (if bulb.on then "ON " else "OFF"))

;; c.add_event_listener bulb_listener

let w = hpair w1 (hpair (space (10,10)) (border on_off_button))

;; Eventloop.run w 

