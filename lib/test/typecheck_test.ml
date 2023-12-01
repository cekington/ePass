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