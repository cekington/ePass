open Core 

module E = Extsyn
module I = Intsyn
module S = Statics

let try_typecheck (s : string) : string = 
  try 
    let () = Util.reset () in 
    let ast = Parse.parse s in
    let ist = Elab.elab ast in 
    let () = S.typecheck ist ist in
    "Typecheck successful"
  with e -> 
    Exn.to_string e

let%expect_test "Test typecheck 1" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       proc dropAll (x : bool) [y : bool] = 
          cancel x
      "
  in print_endline (try_typecheck program);
  [%expect{| (Failure "In process dropAll, channel y not used") |}]
;;

let%expect_test "Test typecheck 2" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       exnproc dropAll (x : bool) [y : bool] = 
          cancel x; cancel y
      "
  in print_endline (try_typecheck program);
  [%expect{| (Failure "In process dropAll, exceptional channel not used") |}]
;;

let%expect_test "Test typecheck 3" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       exnproc dropAll (x : bool) [y : bool] = 
          raise(cancel x; cancel y)
       exec dropAll
      "
  in print_endline (try_typecheck program);
  [%expect{| (Failure "Exec exceptional process dropAll") |}]
;;

let%expect_test "Test typecheck 4" =
  let program =
      " proc dropAll (x : 1) [] = send x ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 5" =
  let program =
      " proc dropAll (x : 1) [y : 1] = send y ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process dropAll, channel y is in antecedent, cannot send unit to it") |}]
;;

let%expect_test "Test typecheck 6" =
  let program =
      " proc test6 (x : 1, y : 1) [] = send x (); send y ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process test6, channel x is unit type, cannot have continue process") |}]
;;

let%expect_test "Test typecheck 7" =
  let program =
      " 
      type bool = +{'true : 1, 'false : 1}
      proc test7 (x : bool) [] = send x 'true
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure "In process test7, channel x not used") |}]
;;

let%expect_test "Test typecheck 8" =
  let program =
      " 
      type bool = +{'true : 1, 'false : 1}
      proc test8 (x : bool) [] = send x 'true; send x ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;