open OUnit2

let () = run_test_tt_main (
  "All tests" >::: [
    Test_property.tests;
    Test_unit.tests
  ]
)