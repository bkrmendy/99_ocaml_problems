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

let test_pack _ = 
  assert_equal [] (Lib_ocaml_problems.pack []);
  assert_equal [[1]] (Lib_ocaml_problems.pack [1]);
  assert_equal [[1; 1]] (Lib_ocaml_problems.pack [1; 1]);
  assert_equal [[1]; [2]] (Lib_ocaml_problems.pack [1; 2]);
  assert_equal [[1; 1]; [2]] (Lib_ocaml_problems.pack [1; 1; 2]);
  assert_equal [[1; 1]; [2]; [1]] (Lib_ocaml_problems.pack [1; 1; 2; 1]);
  assert_equal [[1; 1]; [2; 2]; [3; 3]] (Lib_ocaml_problems.pack [1; 1; 2; 2; 3; 3]);
  assert_equal
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    (Lib_ocaml_problems.pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"])

let test_encode _ =
  assert_equal [] (Lib_ocaml_problems.encode []);
  assert_equal [(1, 1)] (Lib_ocaml_problems.encode [1]);
  assert_equal [(2, 1)] (Lib_ocaml_problems.encode [1; 1]);
  assert_equal [(1, 1); (1, 2)] (Lib_ocaml_problems.encode [1; 2]);
  assert_equal [(1, 1); (2, 2)] (Lib_ocaml_problems.encode [1; 2; 2]);
  assert_equal [(1, 1); (2, 2); (1, 3)] (Lib_ocaml_problems.encode [1; 2; 2; 3]);
  assert_equal
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
    (Lib_ocaml_problems.encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])

let test_encode_adt _ =
  assert_equal [] (Lib_ocaml_problems.encode_adt []);
  assert_equal [Single 1] (Lib_ocaml_problems.encode_adt [1]);
  assert_equal [Multiple (2, "a")] (Lib_ocaml_problems.encode_adt ["a"; "a"]);
  assert_equal [Single 1; Single 2] (Lib_ocaml_problems.encode_adt [1; 2]);
  assert_equal [Single 1; Multiple (2, 2)] (Lib_ocaml_problems.encode_adt [1; 2; 2]);
  assert_equal [Single 1; Multiple (2, 2); Single 3] (Lib_ocaml_problems.encode_adt [1; 2; 2; 3]);
  assert_equal
    [Multiple (4, "a"); Single "b"; Multiple (2, "c"); Multiple (2, "a"); Single "d"; Multiple (4, "e")]
    (Lib_ocaml_problems.encode_adt ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])

let test_decode _ =
  assert_equal [] (Lib_ocaml_problems.decode []);
  assert_equal [1] (Lib_ocaml_problems.decode [Single 1]);
  assert_equal ["a"; "a"] (Lib_ocaml_problems.decode [Multiple (2, "a")]);
  assert_equal [1; 2] (Lib_ocaml_problems.decode [Single 1; Single 2]);
  assert_equal [1; 2; 2] (Lib_ocaml_problems.decode [Single 1; Multiple (2, 2)]);
  assert_equal [1; 2; 2; 3] (Lib_ocaml_problems.decode [Single 1; Multiple (2, 2); Single 3]);
  assert_equal
  ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
    (Lib_ocaml_problems.decode [Multiple (4, "a"); Single "b"; Multiple (2, "c"); Multiple (2, "a"); Single "d"; Multiple (4, "e")])

let test_duplicate _ = 
  assert_equal [] (Lib_ocaml_problems.duplicate []);
  assert_equal [1; 1] (Lib_ocaml_problems.duplicate [1]);
  assert_equal ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] (Lib_ocaml_problems.duplicate ["a";"b";"c";"c";"d"])

let test_replicate _ = 
  assert_equal [] (Lib_ocaml_problems.replicate [1;2;3;4;5] (-2));
  assert_equal [] (Lib_ocaml_problems.replicate [] 0);
  assert_equal [] (Lib_ocaml_problems.replicate [] 1);
  assert_equal (Lib_ocaml_problems.duplicate [1]) (Lib_ocaml_problems.replicate [1] 2);
  assert_equal [3; 3; 3] (Lib_ocaml_problems.replicate [3] 3);
  assert_equal ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] (Lib_ocaml_problems.replicate ["a"; "b"; "c"] 3)

let test_drop_nth _ =
  assert_equal [] (Lib_ocaml_problems.drop_nth [] 0);
  assert_equal [] (Lib_ocaml_problems.drop_nth [1] 0);
  assert_equal [1] (Lib_ocaml_problems.drop_nth [1; 2] 1);
  assert_equal ["a"; "b"; "c"; "e"; "f"; "g"; "i"; "j"] (Lib_ocaml_problems.drop_nth ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)

let test_split _ =
  assert_equal ([], []) (Lib_ocaml_problems.split [] 0);
  assert_equal ([], []) (Lib_ocaml_problems.split [] 1);
  assert_equal ([1], [2]) (Lib_ocaml_problems.split [1; 2] 1);
  assert_equal ([1], [2; 3]) (Lib_ocaml_problems.split [1; 2; 3] 1);
  assert_equal (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) (Lib_ocaml_problems.split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3);
  assert_equal (["a"; "b"; "c"; "d"], []) (Lib_ocaml_problems.split ["a";"b";"c";"d"] 5)

let test_slice _ =
  assert_equal [] (Lib_ocaml_problems.slice [] 2 6);
  assert_equal ["c"; "d"; "e"; "f"; "g"] (Lib_ocaml_problems.slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6)

let test_rotate _ =
  assert_equal [1;2;3;4] (Lib_ocaml_problems.rotate [1;2;3;4] 0);
  assert_equal [2;3;4;1] (Lib_ocaml_problems.rotate [1;2;3;4] 1);
  assert_equal [4;1;2;3] (Lib_ocaml_problems.rotate [1;2;3;4] (-1));
  assert_equal ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] (Lib_ocaml_problems.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);
  assert_equal ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] (Lib_ocaml_problems.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2))

let test_remove_at _ =
  assert_equal [] (Lib_ocaml_problems.remove_at [] 0);
  assert_equal [] (Lib_ocaml_problems.remove_at [] 1);
  assert_equal [] (Lib_ocaml_problems.remove_at [1] 0);
  assert_equal [1] (Lib_ocaml_problems.remove_at [1] 1);
  assert_equal [2] (Lib_ocaml_problems.remove_at [1; 2] 0);
  assert_equal [1; 3] (Lib_ocaml_problems.remove_at [1; 2; 3] 1);
  assert_equal ["a"; "c"; "d"] (Lib_ocaml_problems.remove_at ["a";"b";"c";"d"] 1)

let test_insert_at _ =
  assert_equal [] (Lib_ocaml_problems.insert_at 1 1 []);
  assert_equal [1] (Lib_ocaml_problems.insert_at 1 0 []);
  assert_equal [1;5;2] (Lib_ocaml_problems.insert_at 5 1 [1; 2]);
  assert_equal ["a"; "alfa"; "b"; "c"; "d"] (Lib_ocaml_problems.insert_at "alfa" 1 ["a";"b";"c";"d"]);
  assert_equal ["a"; "b"; "c"; "alfa"; "d"] (Lib_ocaml_problems.insert_at "alfa" 3 ["a";"b";"c";"d"]);
  assert_equal ["a"; "b"; "c"; "d"; "alfa"] (Lib_ocaml_problems.insert_at "alfa" 4 ["a";"b";"c";"d"])

let test_range _ = 
  assert_equal [1] (Lib_ocaml_problems.range 1 1);
  assert_equal [1; 2] (Lib_ocaml_problems.range 1 2);
  assert_equal [1; 2; 3] (Lib_ocaml_problems.range 1 3);
  assert_equal [2; 1] (Lib_ocaml_problems.range 2 1);
  assert_equal [3; 2; 1] (Lib_ocaml_problems.range 3 1);
  assert_equal [4; 5; 6; 7; 8; 9] (Lib_ocaml_problems.range 4 9);
  assert_equal [9; 8; 7; 6; 5; 4] (Lib_ocaml_problems.range 9 4)

let test_extract _ =
  assert_equal
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
    (Lib_ocaml_problems.extract 2 ["a";"b";"c";"d"])
  
let tests = 
  "Unit tests for lists" >::: [
    "test_list_last" >:: test_list_last;
    "test_penultimate" >:: test_penultimate;
    "test_kth_element" >:: test_kth_element;
    "test_length" >:: test_length;
    "test_reverse" >:: test_reverse;
    "test_palindrome" >:: test_palindrome;
    "test_flatten" >:: test_flatten;
    "test_compress" >:: test_compress;
    "test_pack" >:: test_pack;
    "test_encode" >:: test_encode;
    "test_encode_adt" >:: test_encode_adt;
    "test_decode" >:: test_decode;
    "test_duplicate" >:: test_duplicate;
    "test_replicate" >:: test_replicate;
    "test_drop_nth" >:: test_drop_nth;
    "test_split" >:: test_split;
    "test_slice" >:: test_slice;
    "test_rotate" >:: test_rotate;
    "test_remove_at" >:: test_remove_at;
    "test_insert_at" >:: test_insert_at;
    "test_range" >:: test_range;
    "test_extract" >:: test_extract
  ]