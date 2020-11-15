(* Lightbulb example using checkboxes.
   This application will not work until you finish Task 5. *)
;; open Widget
;; open Gctx

(* Make a lightbulb widget controlled by a checkbox's state. *)
let mk_state_lightbulb () : widget =

  let (switch_w, switch_cb) =
    checkbox false "STATE LIGHT" in

  (* A function to display the bulb  *)
  let paint_bulb (g:gctx) : unit =
    let g_new = with_color g
        (if switch_cb.get_value ()
         then yellow
         else black) in
    fill_rect g_new (0, 0) (99, 99)
  in

  let (bulb, _) = canvas (100,100) paint_bulb
  in
  hpair bulb switch_w

(* Make a lightbulb that registers an item listener with the checkbox *)
let mk_listener_lightbulb () : widget =
  let is_on = ref false in

  let (switch_w, switch_cb) =
    checkbox false "LISTENER LIGHT" in

  switch_cb.add_change_listener (fun b -> is_on.contents <- b);

  (* A function to display the bulb  *)
  let paint_bulb (g:gctx) : unit =
    let g_new = with_color g
        (if is_on.contents
         then yellow
         else black) in
    fill_rect g_new (0, 0) (99, 99)
  in

  let (bulb, _) = canvas (100,100) paint_bulb
  in
  hpair bulb switch_w

(* master widget *)
let w = hpair (border (mk_state_lightbulb ()))
          (hpair (space (10,10))
             (border (mk_listener_lightbulb ())))

(** Run the event loop to process user events. *)
;; Eventloop.run w
