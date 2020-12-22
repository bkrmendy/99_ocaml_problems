open OUnit2

let test_example _ = assert_equal 1 1

let suite = 
  "ExampleTests" >::: [
    "test_example" >:: test_example;
  ]

let () = run_test_tt_main suite