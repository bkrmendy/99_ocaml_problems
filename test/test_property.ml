open OUnit2
open Lib_ocaml_problems

let reverse_prop =
  QCheck.Test.make
    ~count:1000
    ~name:"reverse_is_involutive"
    QCheck.(list small_nat)
    (fun l -> Lib_ocaml_problems.reverse (Lib_ocaml_problems.reverse l) = l);;

let tests = 
  "Property tests for lists" >::: (
       List.map QCheck_ounit.to_ounit2_test [
        reverse_prop
       ])