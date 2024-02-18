open Core 

module E = Extsyn
module I = Intsyn
module S = Statics

let try_typecheck (s : string) : string = 
  try 
    let () = Util.reset () in 
    let ast = Parse.parse s in
    let ist = Elab.elab ast in 
    let () = S.typecheck ist in
    "Typecheck successful"
  with e -> 
    Exn.to_string e

let%expect_test "Test typecheck 1" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       proc test1 (x : bool) [y : bool] = 
          cancel x
      "
  in print_endline (try_typecheck program);
  [%expect{| (Failure "In process test1, channel y not used") |}]
;;

let%expect_test "Test typecheck 2" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       exnproc test2 (x : bool) [y : bool] = 
          cancel x; cancel y
      "
  in print_endline (try_typecheck program);
  [%expect{| Typecheck successful |}]
;;

let%expect_test "Test typecheck 3" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       exnproc test3 (x : bool) [y : bool] = 
          raise(cancel x; cancel y)
       exec test3
      "
  in print_endline (try_typecheck program);
  [%expect{| (Failure "Exec exceptional process test3") |}]
;;

let%expect_test "Test typecheck 4" =
  let program =
      " proc test4 (x : 1) [] = send x ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 5" =
  let program =
      " proc test5 (x : 1) [y : 1] = send y ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process test5, channel y is in antecedent, cannot send unit to it") |}]
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
      type bool = &{'true : 1, 'false : 1}
      proc test8 () [x : bool] = send x 'true; send x ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process test8, channel x is in antecedent, cannot send unit to it") |}]
;;

let%expect_test "Test typecheck 9" =
  let program =
      " 
      type pair = 1 * 1
      proc test9 (y : 1) [x : pair] = send x y; send x ()
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process test9, channel x of tensor type is in antecedent, cannot send channel y to it") |}]
;;

let%expect_test "Test typecheck 10" =
  let program =
      " 
      type bool = +{'true : 1, 'false : 1}
      type tribool = 1 * bool
      proc test10 (x : tribool) [y : 1] =
        send x y; send x 'true; send x () 
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 11" =
  let program =
      " 
      proc test11 (y : 1, z : 1) [x : 1 @ 1] =
        send x y; fwd z x
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 12" =
  let program =
      " 
      proc test12 (z : 1) [y : 1] =
        fwd y z
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process test12, cannot forward succedent channel y to antecedent channel z") |}]
;;

let%expect_test "Test typecheck 13" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       proc test13 (x : bool) [y : bool] = 
          raise(cancel x; cancel y)
      "
  in print_endline (try_typecheck program);
  [%expect{|
    (Failure
      "In process test13, raise does not have its corresponding exceptional channel") |}]
;;

let%expect_test "Test typecheck 14" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
       proc test14 (x : bool, y : bool) [] = 
          u : 1 <<- (cancel u; cancel x) catch (send y 'true; cancel u; send y ())
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 15" =
  let program =
      "proc test15 (z : 1) [x : 1 -o 1, y : 1] = 
          send x y; fwd z x
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 16" =
  let program =
      "type bool = +{ 'false : 1, 'true : 1 }
      
      proc not (c : bool) [a : bool] = 
        recv a (
          'false => send c 'true; fwd c a
        | 'true => send c 'false; fwd c a
        )
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 17" =
  let program =
      "type bin = +{ 'e : 1, 'b0 : bin, 'b1 : bin }
      
      proc zero (x : bin) [] = send x 'e ; send x ()
      proc succ (y : bin) [x : bin] =
        recv x ( 'e => recv x (() => send y 'b1 ; send y 'e ; send y ())
               | 'b0 => send y 'b1 ; fwd y x
               | 'b1 => send y 'b0 ; call succ (y) [x] )
      
      proc pred (y : bin) [x : bin] =
        recv x (
          'e => send y 'e; fwd y x
        | 'b0 => send y 'b1; call pred (y) [x]
        | 'b1 => recv x (
            'e => send y 'e; fwd y x 
          | 'b0 => send y 'b0; send y 'b0; fwd y x 
          | 'b1 => send y 'b0; send y 'b1; fwd y x 
          )
        )
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 18" =
  let program =
      "type bin = +{ 'e : 1, 'b0 : bin, 'b1 : bin }
      type list = +{ 'nil : 1, 'cons : bin * list }
      
      proc nil (r : list) [] = send r 'nil ; send r ()
      proc cons (r : list) [x : bin, l : list] =
        send r 'cons; send r x; fwd r l
      
      proc append (r : list) [l : list, k : list] =
        recv l ('nil => recv l (() => fwd r k)
               |'cons => recv l (x => 
                  send r 'cons; 
                  send r x; 
                  call append (r) [l, k]))
      
      proc reverse (r : list) [l : list] =
        recv l (
          'nil => send r 'nil; fwd r l
        | 'cons => recv l (x => 
            empty : list <- call nil (empty) [];
            lx : list <- call cons (lx) [x, empty];
            lrev : list <- call reverse (lrev) [l];
            call append (r) [lrev, lx]
          )  
        )

      type iterator = &{ 'next : bin -o (bin * iterator) , 'done : 1 }

      proc map (r : list) [iter : iterator, l : list] = 
        recv l (
          'nil => send iter 'done; 
            recv iter (() =>  
              send r 'nil; 
              fwd r l
            )
        | 'cons => send iter 'next; 
            recv l (x => 
              send iter x; 
              recv iter (xx =>
                send r 'cons;
                send r xx;
                call map (r) [iter, l]
              )
            ) 
        )
      
      proc isucc (i : iterator) [] = 
        recv i ( 
          'next => recv i (x => 
            y : bin <- call succ (y) [x];
            send i y;
            call isucc (i) []
          )
        | 'done => send i ()
        )

      proc succ (y : bin) [x : bin] =
      recv x ( 'e => recv x (() => send y 'b1 ; send y 'e ; send y ())
              | 'b0 => send y 'b1 ; fwd y x
              | 'b1 => send y 'b0 ; call succ (y) [x])
      "
  in print_endline (try_typecheck program);
  [%expect{|
    Typecheck successful |}]
;;

let%expect_test "Test typecheck 19" =
  let program =
      "type bool = +{'true : 1, 'false : 1}
      exnproc test19 (x : bool) [y : bool] = 
        raise(cancel x; cancel y)
      "
  in print_endline (try_typecheck program);
  [%expect{| Typecheck successful |}]
;;