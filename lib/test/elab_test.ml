module E = Extsyn
module I = Intsyn

open Core

let try_elab (s : string) : string = 
  try 
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
    type %tp_1 = 1
    type nat = {'zero : %tp_1 + 'succ : nat}
    type %tp_2 = tritree * tritree
    type %tp_3 = tritree * %tp_2
    type %tp_4 = nat * %tp_3
    type tritree = {'lead : %tp_1 + 'node : %tp_4} |}]
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
    type %tp_5 = 1
    type bin = {'b0 : bin + 'b1 : bin + 'e : %tp_5}
    type %tp_6 = bin -o store
    type %tp_7 = bin * store
    type %tp_8 = {'none : %tp_5 + 'some : %tp_7}
    type store = {'ins : %tp_6 & 'del : %tp_8}
    proc empty_queue (s : {'ins : %tp_6 & 'del : %tp_8}) [] = recv s ('ins => recv s (x => t <- (call empty_queue (t) []); call node_queue (s) [x, t]) | 'del => send s 'none; send s ())
    proc node_queue (s : {'ins : %tp_6 & 'del : %tp_8}) [x : {'b0 : bin + 'b1 : bin + 'e : %tp_5}, t : {'ins : %tp_6 & 'del : %tp_8}] = recv s ('ins => recv s (y => ss <- (call node_queue (ss) [x, t]); call node_queue (s) [y, ss]) | 'del => send t 'del; recv t ('none => recv t (() => send s 'some; send s x; call empty_queue (s) []) | 'some => recv t (y => send s 'some; send s y; call node_queue (s) [x, t]))) |}]
;;