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

let test_kth_element _ =
  assert_equal None (Lib_ocaml_problems.kth_element [] 0);
  assert_equal None (Lib_ocaml_problems.kth_element [] 1);
  assert_equal None (Lib_ocaml_problems.kth_element [1; 2] 3);
  assert_equal None (Lib_ocaml_problems.kth_element [1; 2] 2);
  assert_equal None (Lib_ocaml_problems.kth_element [1; 2] (-1));
  assert_equal (Some 1) (Lib_ocaml_problems.kth_element [1; 2] 0);
  assert_equal (Some 2) (Lib_ocaml_problems.kth_element [1; 2] 1)
  
let suite = 
  "OcamlProblemsTests" >::: [
    "test_list_last" >:: test_list_last;
    "test_penultimate" >:: test_penultimate;
    "test_kth_element" >:: test_kth_element
  ]

let () = run_test_tt_main suite