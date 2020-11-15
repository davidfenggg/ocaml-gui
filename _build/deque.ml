(** Deque collection library *)

(* You do not need to modify this file. *)

(** A deque is like a queue but can add or remove elements
    on either end. This implementation of deques, based on
    lists, gets the job done. However, your implementation from
    the queues homework is much more efficient.
*)

type 'a deque = { mutable contents : 'a list }

(** create a new container *)
let create () : 'a deque =
  { contents = []; }

(** is the container empty *)
let is_empty (d : 'a deque) : bool =
  d.contents = []

(** add to the front of the deque *)
let insert_head (x: 'a) (q: 'a deque) : unit =
  q.contents <- (x :: q.contents)

(** add to the back of the deque *)
let insert_tail (x: 'a) (q: 'a deque) : unit =
  q.contents <- q.contents @ [x]

(** remove the first element from the deque and return it,
    fail if the queue is empty *)
let remove_head (q: 'a deque) : 'a =
  begin match q.contents with
    | h :: rest -> (q.contents <- rest; h)
    | [] -> failwith "empty deque"
  end

(** remove the last element from the deque and return it
    fail if the queue is empty *)
let remove_tail (q: 'a deque) : 'a =
  let rec last (l : 'a list) (acc : 'a list) :'a list * 'a =
    begin match l with
      | x :: [] -> acc , x
      | x :: tl -> last tl (x :: acc)
      | [] -> failwith "empty deque"
    end in
  let (c, x) = last q.contents [] in
  q.contents <- List.rev c;
  x

(** run a command c for each node of the deque in order
    of head to tail *)
let iterate (c:'a -> unit) (q:'a deque) : unit =
  let rec loop (l:'a list) : unit =
    begin match l with
      | [] -> ()
      | x :: rest -> c x; loop rest
    end
  in loop q.contents
