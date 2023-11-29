module E = Extsyn
module I = Intsyn

open Core

let try_elab (s : string) : string = 
  try 
    let () = I.reset_counter () in 
    let ast = Parse.parse s in
    let ist = I.elab ast in 
      I.Print.pp_prog ist
  with e -> 
    Exn.to_string e

let%expect_test "Test elab 1" =
  let program =
      "type tritree = +{'lead : 1, 'node : nat * tritree * tritree * tritree}"
  in print_endline (try_elab program);
  [%expect{| (Failure "Type variable nat is not defined") |}]
;;

let%expect_test "Test elab 2" =
  let program =
      "type nat = +{'zero : 1, 'succ : nat}
      type tritree = +{'lead : 1, 'node : nat * tritree * tritree * tritree}"
  in print_endline (try_elab program);
  [%expect{|
    type %tp_0 = 1
    type nat = {'zero : %tp_0 + 'succ : nat}
    type %tp_1 = tritree * tritree
    type %tp_2 = tritree * %tp_1
    type %tp_3 = nat * %tp_2
    type tritree = {'lead : %tp_0 + 'node : %tp_3} |}]
;;

let%expect_test "Test elab 3" =
  let program =
      "
      type bin = +{'b0 : bin, 'b1 : bin, 'e : 1}
      type store = &{ 'ins : bin -o store, 'del : +{ 'none : 1, 'some : bin * store } }
      proc empty_queue (s : store) [] = 
        recv s (
        'ins => recv s (x => t <- call empty_queue (t) []; call node_queue (s) [x, t])
        | 'del => send s 'none; send s ()
        )

      proc node_queue (s : store) [x : bin, t : store] = 
        recv s (
          'ins => recv s (y => ss <- call node_queue (ss) [x, t]; call node_queue (s) [y, ss])
          | 'del => send t 'del; 
            recv t (
              'none => recv t (() => send s 'some; send s x; call empty_queue (s) [])
            | 'some => recv t (y => send s 'some; send s y; call node_queue (s) [x, t])
          )
        )
      "
  in print_endline (try_elab program);
  [%expect{|
    type %tp_0 = 1
    type bin = {'b0 : bin + 'b1 : bin + 'e : %tp_0}
    type %tp_1 = bin -o store
    type %tp_2 = bin * store
    type %tp_3 = {'none : %tp_0 + 'some : %tp_2}
    type store = {'ins : %tp_1 & 'del : %tp_3}
    proc empty_queue (s : {'ins : %tp_1 & 'del : %tp_3}) [] = recv s ('ins => recv s (x => t <- (call empty_queue (t) []); call node_queue (s) [x, t]) | 'del => send s 'none; send s ())
    proc node_queue (s : {'ins : %tp_1 & 'del : %tp_3}) [x : {'b0 : bin + 'b1 : bin + 'e : %tp_0}, t : {'ins : %tp_1 & 'del : %tp_3}] = recv s ('ins => recv s (y => ss <- (call node_queue (ss) [x, t]); call node_queue (s) [y, ss]) | 'del => send t 'del; recv t ('none => recv t (() => send s 'some; send s x; call empty_queue (s) []) | 'some => recv t (y => send s 'some; send s y; call node_queue (s) [x, t]))) |}]
;;

let%expect_test "Test elab 4" =
  let program =
      "type bin = +{'e : 1, 'b0 : bin, 'b1 : bin, 'b0 : bin}"
  in print_endline (try_elab program);
  [%expect{|
    (Failure "Duplicate label 'b0") |}]
;;

let%expect_test "Test elab 4" =
  let program =
      "proc fwdd (x : 1) [y : 1] = fwd x y
       exec fwwd"
  in print_endline (try_elab program);
  [%expect{|
    (Failure "Process fwwd is not declared") |}]
;;