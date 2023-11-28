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