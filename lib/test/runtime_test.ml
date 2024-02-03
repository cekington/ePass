open Core 

module E = Extsyn
module I = Intsyn
module S = Statics
module D = Dynamics

let try_runtime (s : string) : string = 
  try 
    let () = Util.reset () in 
    let ast = Parse.parse s in
    let ist = Elab.elab ast in 
    let () = S.typecheck ist ist in
    D.exec ist
  with e -> 
    Exn.to_string e

let%expect_test "Test runtime 1" =
  let program =
      "type bin = +{ 'e : 1, 'b0 : bin, 'b1 : bin }

      proc zero (z : bin) [] = send z 'e ; send z ()
      
      proc succ (y : bin) [x : bin] =
        recv x ( 'e => recv x (() => send y 'b1 ; send y 'e ; send y ())
               | 'b0 => send y 'b1 ; fwd y x
               | 'b1 => send y 'b0 ; call succ (y) [x] )
      
      proc three (n3 : bin) [] = send n3 'b1 ; send n3 'b1 ; send n3 'e ; send n3 ()
      proc four (n4 : bin) [] = n3 : bin <- call three (n3) []; call succ (n4) [n3]

      exec three
      exec four

      type list = +{ 'nil : 1, 'cons : bin * list }
      proc nil (r : list) [] = send r 'nil ; send r ()
      proc cons (r : list) [x : bin, l : list] =
        send r 'cons ; send r x ; fwd r l

      proc append (r : list) [l : list, k : list] =
        recv l ('nil => recv l (() => fwd r k)
               |'cons => recv l (x => send r 'cons ;
                                      send r x ;
                                      call append (r) [l, k]))

      proc test_list (r : list) [] =
        l : list <- call nil (l) [] ;
        n3 : bin <- call three (n3) [] ;
        l3 : list <- call cons (l3) [n3, l] ;
        k : list <- call nil (k) [] ;
        n4 : bin <- call four (n4) [] ;
        k4 : list <- call cons (k4) [n4, k] ;
        call append (r) [l3, k4]

      exec test_list
      
      type store = &{ 'ins : bin -o store,
                      'del : +{ 'none : 1, 'some : bin * store } }
      
      proc empty (s : store) [] =
           recv s ( 'ins => recv s (x => e : store <- call empty (e) [] ;
                                         call node (s) [x, e] )
                  | 'del => send s 'none ; send s () )
      
      proc node (s : store) [x : bin, t : store] =
           recv s ( 'ins => recv s (y => s' : store <- call node (s') [x, t] ;
                                         call node (s) [y, s'])
                  | 'del => send s 'some ;
                            send s x ;
                            fwd s t )
      
      proc store2list (l : list) [s : store] =
           send s 'del ;
           recv s ( 'none => recv s (() => send l 'nil ; send l ())
                  | 'some => recv s (x => send l 'cons ; send l x ; call store2list (l) [s]) )
      
      proc test_store (l : list) [] =
           s : store <- call empty (s) [] ;
           n3 : bin <- call three (n3) [] ;
           send s 'ins ; send s n3 ;
           n4 : bin <- call four (n4) [] ;
           send s 'ins ; send s n4 ;
           call store2list (l) [s]
      
      exec test_store     
      "
  in print_endline (try_runtime program);
  [%expect{|
    Executing process three:
    #0 -> 'b1.'b1.'e.()

    Executing process four:
    #0 -> 'b0.'b0.'b1.'e.()

    Executing process test_list:
    #0 -> 'cons.#2.'cons.#5.'nil.()
    #5 -> 'b0.'b0.'b1.'e.()
    #2 -> 'b1.'b1.'e.()

    Executing process test_store:
    #0 -> 'cons.#4.'cons.#2.'nil.()
    #4 -> 'b0.'b0.'b1.'e.()
    #2 -> 'b1.'b1.'e.() |}]
;;

let%expect_test "Test runtime 2" =
  let program =
      "type bin = +{ 'e : 1, 'b0 : bin, 'b1 : bin }

      proc succ (y : bin) [x : bin] =
      recv x ( 'e => recv x (() => send y 'b1 ; send y 'e ; send y ())
              | 'b0 => send y 'b1 ; fwd y x
              | 'b1 => send y 'b0 ; call succ (y) [x] )
      
      proc zero (n0 : bin) [] = send n0 'e ; send n0 ()
      proc one (n1 : bin) [] = n0 : bin <- call zero (n0) []; call succ (n1) [n0]
      proc two (n2 : bin) [] = n1 : bin <- call one (n1) []; call succ (n2) [n1]
      proc three (n3 : bin) [] = n2 : bin <- call two (n2) []; call succ (n3) [n2]
      proc four (n4 : bin) [] = n3 : bin <- call three (n3) []; call succ (n4) [n3]
      
      type list = +{ 'nil : 1, 'cons : bin * list }
      proc nil (r : list) [] = send r 'nil ; send r ()
      proc cons (r : list) [x : bin, l : list] =
        send r 'cons ; send r x ; fwd r l
      
      proc dupbin (n1 : bin, n2 : bin) [n : bin] =
        recv n ( 'e => recv n (() => send n1 'e ; send n1 () ; send n2 'e ; send n2 ())
                | 'b0 => send n1 'b0 ; send n2 'b0 ; call dupbin (n1, n2) [n]
                | 'b1 => send n1 'b1 ; send n2 'b1 ; call dupbin (n1, n2) [n] )
      
      proc dupbin_test (result1 : bin, result2 : bin) [] =
        n1 : bin <- call one (n1) [];
        call dupbin (result1, result2) [n1]
      
      exec dupbin_test

      proc list_test (result : list) [] =
        l : list <- call nil (l) [] ;
        n0 : bin <- call zero (n0) [] ;
        ln0 : list <- call cons (ln0) [n0, l] ;
        n1 : bin <- call one (n1) [] ;
        ln0n1 : list <- call cons (ln0n1) [n1, ln0] ;
        n2 : bin <- call two (n2) [] ;
        call cons (result) [n2, ln0n1]

      exec list_test

      proc duplist (result1 : list, result2 : list) [l : list] =
        recv l ( 'nil => recv l (() => send result1 'nil ; send result1 () ; send result2 'nil ; send result2 ())
                | 'cons => recv l (x => 
                    n1 : bin <- (
                      n2 : bin <- call dupbin (n1, n2) [x];
                      send result2 'cons; send result2 n2
                    );
                    send result1 'cons; send result1 n1; call duplist (result1, result2) [l]
                  )
                )

      proc duplist_test (result1 : list, result2 : list) [] =
        l : list <- call list_test (l) [];
        call duplist (result1, result2) [l]

      exec duplist_test
      "
  in print_endline (try_runtime program);
  [%expect{|
    Executing process dupbin_test:
    #1 -> 'b1.'e.()
    #0 -> 'b1.'e.()

    Executing process list_test:
    #0 -> 'cons.#6.'cons.#4.'cons.#2.'nil.()
    #4 -> 'b1.'e.()
    #6 -> 'b0.'b1.'e.()
    #2 -> 'e.()

    Executing process duplist_test:
    #15 -> 'b1.'e.()
    #17 -> 'e.()
    #1 -> 'cons.#13.'cons.#15.'cons.#17.'nil.()
    #0 -> 'cons.#11.'cons.#14.'cons.#16.'nil.()
    #11 -> 'b0.'b1.'e.()
    #13 -> 'b0.'b1.'e.()
    #16 -> 'e.()
    #14 -> 'b1.'e.() |}]
;;