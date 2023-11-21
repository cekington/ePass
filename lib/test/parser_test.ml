open Core

module E = Extsyn

let parse (s : string) : E.prog =
  let lexbuf = Lexing.from_string s in
    Parser.prog Lexer.read lexbuf

let%expect_test "Test parsing 1" =
  let program =
      "type bool = +{'true : 1, 'false : 1}"
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| type bool = {'true : 1 + 'false : 1} |}]
;;

let%expect_test "Test parsing 2" =
  let program =
      "proc rev (c : bool) [a : bool] =
        recv a (
          'true => send c 'false; fwd c a
        | 'false => send c 'true; fwd c a)
      "
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| proc rev (c : bool) [a : bool] = recv a ('true => send c 'false; fwd c a | 'false => send c 'true; fwd c a) |}]
;;

let%expect_test "Test parsing 3" =
  let program =
      "proc pred (x : std) [y : pos] = 
        recv y (
          'b0 => send x 'b1; call pred (x) [y]
        | 'b1 => recv y (
            'e => send x 'e; fwd x y
          | 'b0 => send x 'b0; send x 'b0; fwd x y
          | 'b1 => send x 'b0; send x 'b1; fwd x y
          )
        )
      "
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| proc pred (x : std) [y : pos] = recv y ('b0 => send x 'b1; call pred (x) [y] | 'b1 => recv y ('e => send x 'e; fwd x y | 'b0 => send x 'b0; send x 'b0; fwd x y | 'b1 => send x 'b0; send x 'b1; fwd x y)) |}]
;;


let%expect_test "Test parsing 4" =
  let program =
      "proc drop () [a : bool] = cancel a"
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| proc drop () [a : bool] = cancel a |}]
;;

let%expect_test "Test parsing 5" =
  let program =
      "proc inf () [a : bool] = call inf () [a]"
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| proc inf () [a : bool] = call inf () [a] |}]
;;

let%expect_test "Test parsing 6" =
  let program =
      "proc drop (a : nat) [b : nat] = raise cancel a; cancel b"
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| proc drop (a : nat) [b : nat] = raise (cancel a; cancel b) |}]
;;

let%expect_test "Test parsing 7" =
  let program =
      "proc inf (c : bool) [a : bool] = 
      try call drop (a) [] catch send c 'false; send c ()"
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{| proc inf (c : bool) [a : bool] = try (call drop (a) []) catch (send c 'false; send c ()) |}]
;;

let%expect_test "Test parsing 8" =
  let program =
      "type store = &{ 'ins : bin -o store, 'del : +{ 'none : 1, 'some : bin * store } }
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
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{|
    type store = {'ins : bin -o store & 'del : {'none : 1 + 'some : (bin * store)}}
    proc empty_queue (s : store) [] = recv s ('ins => recv s (x => cut t<-(call empty_queue (t) []); call node_queue (s) [x, t]) | 'del => send s 'none; send s ())
    proc node_queue (s : store) [x : bin, t : store] = recv s ('ins => recv s (y => cut ss<-(call node_queue (ss) [x, t]); call node_queue (s) [y, ss]) | 'del => send t 'del; recv t ('none => recv t (() => send s 'some; send s x; call empty_queue (s) []) | 'some => recv t (y => send s 'some; send s y; call node_queue (s) [x, t]))) |}]
;;

let%expect_test "Test parsing 9" =
  let program =
      "exec three
       fail exnproc error (a : 1) [] = send a ()
      "
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect{|
    exec three
    fail exnproc error (a : 1) [] = send a () |}]
;;


