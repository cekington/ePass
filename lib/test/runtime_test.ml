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
    #2 -> 'b1.'b1.'e.()
    #4 -> 'b0.'b0.'b1.'e.() |}]
;;