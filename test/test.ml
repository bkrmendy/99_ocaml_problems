open OUnit2
open Lib_ocaml_problems

let test_list_last _ =
  assert_equal None (Lib_ocaml_problems.last []);
  assert_equal (Some 1) (Lib_ocaml_problems.last [1]);
  assert_equal (Some 2) (Lib_ocaml_problems.last [1 ; 2]);
  assert_equal (Some "d") (Lib_ocaml_problems.last [ "a" ; "b" ; "c" ; "d" ])

let test_penultimate _ = 
  assert_equal None (Lib_ocaml_problems.penultimate []);
  assert_equal None (Lib_ocaml_problems.penultimate [1]);
  assert_equal (Some 1) (Lib_ocaml_problems.penultimate [1; 2]);
  assert_equal (Some 2) (Lib_ocaml_problems.penultimate [1; 2; 3]);
  assert_equal (Some "c") (Lib_ocaml_problems.penultimate [ "a" ; "b" ; "c" ; "d" ])

let test_kth_element _ =
  assert_equal None (Lib_ocaml_problems.kth_element [] 0);
  assert_equal None (Lib_ocaml_problems.kth_element [] 1);
  assert_equal None (Lib_ocaml_problems.kth_element [1; 2] 3);
  assert_equal None (Lib_ocaml_problems.kth_element [1; 2] 2);
  assert_equal None (Lib_ocaml_problems.kth_element [1; 2] (-1));
  assert_equal (Some 1) (Lib_ocaml_problems.kth_element [1; 2] 0);
  assert_equal (Some 2) (Lib_ocaml_problems.kth_element [1; 2] 1);
  assert_equal (Some "c") (Lib_ocaml_problems.kth_element [ "a" ; "b"; "c"; "d"; "e" ] 2);
  assert_equal None (Lib_ocaml_problems.kth_element [ "a" ] 3)

let test_length _ = 
  assert_equal 0 (Lib_ocaml_problems.length []);
  assert_equal 1 (Lib_ocaml_problems.length [1;]);
  assert_equal 2 (Lib_ocaml_problems.length [1; 2]);
  assert_equal 3 (Lib_ocaml_problems.length [ "a" ; "b" ; "c"])

let test_reverse _ =
  assert_equal [] (Lib_ocaml_problems.reverse []);
  assert_equal [1] (Lib_ocaml_problems.reverse [1]);
  assert_equal [2; 1] (Lib_ocaml_problems.reverse [1; 2]);
  assert_equal ["c"; "b"; "a"] (Lib_ocaml_problems.reverse ["a" ; "b" ; "c"])

let test_palindrome _ =
  assert_equal true (Lib_ocaml_problems.is_palindrome [1; 2; 1]);
  assert_equal true (Lib_ocaml_problems.is_palindrome [1; 2; 2; 1]);
  assert_equal true (Lib_ocaml_problems.is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]);
  assert_equal false (Lib_ocaml_problems.is_palindrome [1; 2; 2]);
  assert_equal false (Lib_ocaml_problems.is_palindrome [ "a" ; "b" ])

let test_flatten _ =
  assert_equal [] (Lib_ocaml_problems.flatten []);
  assert_equal [1] (Lib_ocaml_problems.flatten [One 1]);
  assert_equal [1; 2] (Lib_ocaml_problems.flatten [One 1; One 2]);
  assert_equal [1; 2] (Lib_ocaml_problems.flatten [Many [One 1; One 2]]);
  assert_equal ["a"; "b"; "c"; "d"; "e"] (Lib_ocaml_problems.flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])

let test_compress _ =
  assert_equal [] (Lib_ocaml_problems.compress []);
  assert_equal [1] (Lib_ocaml_problems.compress [1]);
  assert_equal [1] (Lib_ocaml_problems.compress [1; 1]);
  assert_equal [1] (Lib_ocaml_problems.compress [1; 1; 1]);
  assert_equal [1; 2] (Lib_ocaml_problems.compress [1; 2]);
  assert_equal [1; 2] (Lib_ocaml_problems.compress [1; 1; 2]);
  assert_equal [1; 2] (Lib_ocaml_problems.compress [1; 1; 2; 2]);
  assert_equal ["a"; "b"; "c"; "a"; "d"; "e"] (Lib_ocaml_problems.compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
  
let suite = 
  "OcamlProblemsTests" >::: [
    "test_list_last" >:: test_list_last;
    "test_penultimate" >:: test_penultimate;
    "test_kth_element" >:: test_kth_element;
    "test_length" >:: test_length;
    "test_reverse" >:: test_reverse;
    "test_palindrome" >:: test_palindrome;
    "test_flatten" >:: test_flatten;
    "test_compress" >:: test_compress
  ]

let () = run_test_tt_main suite