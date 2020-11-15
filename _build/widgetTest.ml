                                            ;; open Assert
;; open Widget

(* Testing code for the widget library. These unit tests make sure
   that the non-visual behavior of the widgets is correctly
   implemented.  *)

(* We give you some unit tests that already pass (for the widgets that
   we have already implemented) and some tests that will only pass
   once you have completed tasks 1 and 5.  *)

(* Create a 'dummy' event *)
let gc = Gctx.top_level
let click55 = Gctx.make_test_event Gctx.MouseDown (5,5)

(* Label widget *)
;; run_test "label creation" (fun () ->
    let _, lc1 = label "l1" in
    lc1.get_label () = "l1")

;; run_test "label string change" (fun () ->
    let _, lc1 = label "l1" in
    lc1.set_label "l2";
    lc1.get_label () = "l2")

;; run_test "label local space" (fun () ->
    let _, lc1 = label "l1" in
    let _, lc2 = label "l2" in
    lc1.set_label "l3";
    lc2.get_label () = "l2")

(* Space widget *)

;; run_test "space size" (fun () ->
    let w1 = space (10,10) in
    w1.size () = (10,10))

(* Border widget *)

;; run_test "border size" (fun () ->
    let w1 = border (space (10,10)) in
    w1.size () = (14,14))

(* Hpair widget *)

;; run_test "hpair size" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = hpair w1 w2 in
    w.size() = (40, 50))

(* Notifiers and event handling for above *)

;; run_test "notifier size" (fun () ->
    let w, _ = notifier (space (10,20)) in
    w.size () = (10,20))

;; run_test "notifier handle (nothing)" (fun () ->
    let w, nc = notifier (space (10,10)) in
    w.handle gc click55;
    true)

;; run_test "notifier handle (mouse down)" (fun () ->
    let w, nc = notifier (space (10,10)) in
    let state = ref false in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

;; run_test "border handle (mouse down)" (fun () ->
    let w1, nc = notifier (space (10,10)) in
    let w = border w1 in
    let state = ref false in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

;; run_test "hpair handle click in left widget" (fun () ->
    let w1, nc = notifier (space (10,20)) in
    let w2 = space (20,30) in
    let w = hpair w1 w2 in
    let state = ref false in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

;; run_test "hpair handle click in right widget" (fun () ->
    let w1 = space (10,20) in
    let w2, nc = notifier (space (20,30)) in
    let w = hpair w1 w2 in
    let state = ref false in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,15));
    state.contents)

;; run_test "hpair handle only one click" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = hpair w1 w2 in
    let state1 = ref false in
    let state2 = ref false in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,15));
    state2.contents && not state1.contents)

;; run_test "hpair handle dead space" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = hpair w1 w2 in
    let state1 = ref false in
    let state2 = ref false in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (5,25));
    not state1.contents && not state1.contents)

(* canvas *)

;; run_test "canvas handle" (fun () ->
    let state = ref false in
    let cw, nc = canvas (10,10) (fun g -> ()) in
    nc.add_event_listener
      (mouseclick_listener (fun () -> state.contents <- true));
    cw.handle gc click55;
    state.contents)

;; run_test "canvas size" (fun () ->
    let cw, nc = canvas (10,10) (fun g -> ()) in
    cw.size () = (14,14))

(* Below, we provide some tests for the widgets that you are required to write
 * as part of this assignment. We will be grading your code against these
 * tests. *)

;; print_endline "--------- Task 1: vpair --------------------"			 
(* [vpair] tests *)


;; run_test "vpair size" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = vpair w1 w2 in
    w.size() = (30, 70))

;; run_test "vpair handle click in top widget" (fun () ->
    let w1, nc = notifier (space (10,20)) in
    let w2 = space (20,30) in
    let w = vpair w1 w2 in
    let state = ref false in
    nc.add_event_listener (mouseclick_listener (fun () -> state.contents <- true));
    w.handle gc click55;
    state.contents)

;; run_test "vpair handle click in bottom widget" (fun () ->
    let w1 = space (10,20) in
    let w2, nc = notifier (space (20,30)) in
    let w = vpair w1 w2 in
    let state = ref false in
    nc.add_event_listener
      (mouseclick_listener
         (fun () -> state.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,25));
    state.contents)

;; run_test "vpair handle only one click" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = vpair w1 w2 in
    let state1 = ref false in
    let state2 = ref false in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (15,25));
    state2.contents && not state1.contents)

;; run_test "vpair handle dead space" (fun () ->
    let w1, nc1 = notifier (space (10,20)) in
    let w2, nc2 = notifier (space (20,30)) in
    let w = vpair w1 w2 in
    let state1 = ref false in
    let state2 = ref false in
    nc1.add_event_listener
      (mouseclick_listener (fun () -> state1.contents <- true));
    nc2.add_event_listener
      (mouseclick_listener (fun () -> state2.contents <- true));
    w.handle gc (Gctx.make_test_event Gctx.MouseDown (25,5));
    not state1.contents && not state1.contents)

(* [list_layout] tests *)

;; print_endline "--------- Task 1: list_layout ------------------"			 



;; run_test "hlist size empty" (fun () ->
    let w = hlist [] in
    w.size () = (0,0))

;; run_test "hlist size nonempty" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = hlist [w1; w2] in
    w.size() = (40, 50))


;; run_test "vlist size empty" (fun () ->
    let w = vlist [] in
    w.size () = (0,0))

;; run_test "vlist size nonempty" (fun () ->
    let w1 = space (10,20) in
    let w2 = space (30,50) in
    let w = vlist [w1; w2] in
    w.size() = (30, 70))

;; print_endline "--------- Task 5: checkbox ---------------"			 


(* [make_controller] tests *)
;; run_test "make_controller get_value returns init value" (fun () ->
    let vc = make_controller 1 in
    vc.get_value () = 1)

;; run_test "make_controller get_value returns correct value after change"
     (fun () ->
       let vc = make_controller 1 in
       let init = vc.get_value () in
       vc.change_value 2;
       init = 1 && vc.get_value () = 2)

;; run_test "make_controller change_listeners are triggered on change"
     (fun () ->
      let vc = make_controller 1 in
      let t = ref 0 in
      vc.add_change_listener (fun v -> t.contents <- v);
      vc.change_value 2;
      t.contents = 2)

(* [checkbox] tests *)
;; run_test "checkbox init true" (fun () ->
    let w, cc = checkbox true "checkbox" in
    cc.get_value ())

;; run_test "checkbox init false" (fun () ->
    let w, cc = checkbox false "checkbox" in
    not (cc.get_value ()))

;; run_test "checkbox click" (fun () ->
    let w, cc = checkbox false "checkbox" in
    w.handle gc click55;
    cc.get_value())

;; run_test "checkbox click click" (fun () ->
    let w, cc = checkbox false "checkbox" in
    w.handle gc click55;
    w.handle gc click55;
    not (cc.get_value()))

;; run_test "checkbox click click click" (fun () ->
    let w, cc = checkbox false "checkbox" in
    w.handle gc click55;
    w.handle gc click55;
    w.handle gc click55;
    (cc.get_value()))

;; run_test "checkbox listener" (fun () ->
    let w, cc = checkbox false "checkbox" in
    let state = ref false in
    cc.add_change_listener (fun b -> state.contents <- b);
    w.handle gc click55;
    state.contents)


