(* A modified version of the DEQUE interface *)

(* Do not  modify this file. *)

type 'a deque

(* create a new container *)
val create : unit -> 'a deque

(* is the container empty? *)
val is_empty : 'a deque -> bool

(* add to the front of the deque *)
val insert_head : 'a -> 'a deque -> unit  

(* add to the back of the deque *)
val insert_tail : 'a -> 'a deque -> unit  

(* remove the first element from the deque and return it *)
(* fail if the queue is empty *)
val remove_head : 'a deque -> 'a  

(* remove the last element from the deque and return it *)
(* fail if the queue is empty *)
val remove_tail : 'a deque -> 'a   

(* run a command c for each node of the deque in order
   of head to tail *)
val iterate : ('a -> unit) -> 'a deque -> unit
