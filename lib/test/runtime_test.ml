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
      "type nat = +{'e : 1, 'succ : nat}

      proc succ (y : nat) [x : nat] =
        send y 'succ; fwd y x
      
      type nlist = +{'nil : 1, 'cons : nat * nlist}
      
      proc nlist_nil (r : nlist) [] = send r 'nil ; send r ()
      
      proc nlist_cons (r : nlist) [x : nat, l : nlist] =
        send r 'cons ; send r x ; fwd r l
      
      type nnlist = +{'nil : 1, 'cons : (nat * nat) * nnlist}
      
      proc nnlist_nil (r : nnlist) [] = send r 'nil ; send r ()
      
      proc nnlist_cons (r : nnlist) [x : nat * nat, l : nnlist] =
        send r 'cons ; send r x ; fwd r l
      
      proc make_pair (r : nat * nat) [x : nat, y : nat] =
        send r x; fwd r y
      
      exnproc list_zip (result : nnlist) [l1 : nlist, l2 : nlist] =
        recv l1 (
        'nil => recv l1 (() => recv l2 (
            'nil => recv l2 (() => send result 'nil; send result ())
          | 'cons => raise (cancel l2; cancel result)
          )) 
        | 'cons => recv l1 (x => recv l2 (
            'nil => raise (cancel l1; cancel l2; cancel x; cancel result)
          | 'cons => recv l2 (y => 
              xy : nat * nat <- call make_pair (xy) [x, y];
              send result 'cons; send result xy; call list_zip (result) [l1, l2]
            )
          ))
        )
      
      exnproc list_zip_v2 (result : nnlist, remain : nlist) [l1 : nlist, l2 : nlist] =
        recv l1 (
          'nil => recv l1 (() => recv l2 (
            'nil => recv l2 (() => cancel remain; send result 'nil; send result ())
          | 'cons => recv l2 (x => raise (cancel result; send remain 'cons; send remain x; fwd remain l2))
          ))
        | 'cons => recv l1 (x => recv l2 (
            'nil => recv l2 (() => raise (cancel result; send remain 'cons; send remain x; fwd remain l1))
          | 'cons => recv l2 (y => 
              xy : nat * nat <- call make_pair (xy) [x, y];
              send result 'cons; send result xy; call list_zip_v2 (result, remain) [l1, l2]
            )
          ))
        )
      
      proc zero (z0 : nat) [] =
        send z0 'e; send z0 ()
      
      proc one (z1 : nat) [] =
        z0 : nat <- call zero (z0) [];
        send z1 'succ; fwd z1 z0
      
      proc two (z2 : nat) [] =
        z1 : nat <- call one (z1) [];
        send z2 'succ; fwd z2 z1
      
      proc test_list_1 (r : nlist) [] =
        z1 : nat <- call one (z1) [];
        z2 : nat <- call two (z2) [];
        send r 'cons; send r z2; send r 'cons; send r z1; send r 'nil; send r ()
        
      proc test_list_2 (r : nlist) [] =
        z0 : nat <- call zero (z0) [];
        z1 : nat <- call one (z1) [];
        z2 : nat <- call two (z2) [];
        send r 'cons; send r z2; send r 'cons; send r z1; 
        send r 'cons; send r z0; send r 'nil; send r ()
      
      proc test_list_zip (r : nnlist, remain : nlist) [] =
        l1 : nlist <- call test_list_1 (l1) [];
        l2 : nlist <- call test_list_2 (l2) [];
        x : 1 <- (try (call list_zip_v2 (r, remain) [l1, l2]) catch (send x ()));
        cancel x
      
      exec test_list_zip
      "
  in print_endline (try_runtime program);
  [%expect{|
    Executing process test_list_zip:
    #18 -> #5.'succ.'e.()
    #1 -> 'cons.#7.'nil.()
    #0 -> 'cons.#16.'cons.#18.cancelled
    #5 -> 'succ.'e.()
    #7 -> 'e.()
    #16 -> #8.'succ.'succ.'e.()
    #8 -> 'succ.'succ.'e.() |}]
;;

let%expect_test "Test runtime 2" =
  let program =
      "type result = 1 @ result

      exnproc server_good (y : 1) [] =
        send y ()
      
      exnproc server_bad (y : 1) [] =
        raise (send y ())
      
      proc client_scene1 (y : result) [] = 
        recv y (x => try call server_good (x) [] catch call client_scene1 (y) [])

      proc client_scene2 (y : result) [] = 
        recv y (x => try call server_bad (x) [] catch call client_scene2 (y) [])
      
      proc client_test_1 (x : result, u : 1) [] =
        y : result <- call client_scene1 (y) [];
        send y u;
        fwd x y
      
      proc client_test_2 (x : result, u : 1) [] =
        y : result <- call client_scene2 (y) [];
        send y u;
        fwd x y

      exec client_test_1

      exec client_test_2
      "
  in print_endline (try_runtime program);
  [%expect{|
    Executing process client_test_1:
    #1 -> ()
    #0 -> cancelled

    Executing process client_test_2:
    #1 -> ()
    #0 -> - |}]
;;