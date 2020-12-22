open OUnit2
open Lib_ocaml_problems

let test_list_last _ =
  assert_equal None (Lib_ocaml_problems.last []);
  assert_equal (Some 1) (Lib_ocaml_problems.last [1]);
  assert_equal (Some 2) (Lib_ocaml_problems.last [1 ; 2])

let test_penultimate _ = 
  assert_equal None (Lib_ocaml_problems.penultimate []);
  assert_equal None (Lib_ocaml_problems.penultimate [1]);
  assert_equal (Some 1) (Lib_ocaml_problems.penultimate [1; 2]);
  assert_equal (Some 2) (Lib_ocaml_problems.penultimate [1; 2; 3])
  
let suite = 
  "OcamlProblemsTests" >::: [
    "test_list_last" >:: test_list_last;
    "test_penultimate" >:: test_penultimate
  ]

let () = run_test_tt_main suite