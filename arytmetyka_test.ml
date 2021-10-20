(* open Arytmetyka *)
open OUnit2

let inf = infinity

let suite =
  "generic_functions"
  >::: [ 
         (* (* ; ("min_max"
         >:: fun _ ->
         assert_raises (Failure "not defined for empty lists") (fun () -> min_max []);
         assert_equal (min_max [ 69 ]) (69, 69);
         assert_equal (min_max [ 69; 69 ]) (69, 69);
         assert_equal (min_max [ -0.; 0. ]) (0., -0.) (* because -0. = +0. *);
         assert_equal (min_max [ 6; 9 ]) (6, 9);
         assert_equal (min_max [ 9; 6 ]) (6, 9)) *)
       ("addition"
         >:: fun _ -> ignore 0)
         (* assert_equal ([ 8., 10. ] +$ [ -1., 1. ]) [ 7., 11. ];
         assert_equal ([ 8., 10. ] +$ [ inf, inf ]) [ inf, inf ];
         assert_equal ([ 0., 0. ] +$ [ -.inf, -1.; 1., inf ]) [ -.inf, -1.; 1., inf ]) *)
       ; ("subtraction"
         >:: fun _ ->ignore 0)
         (* assert_equal ([ 8., 10. ] -$ [ -1., 1. ]) [ 7., 11. ];
         assert_equal ([ 8., 10. ] -$ [ 0., inf ]) [ -.inf, 10. ];
         assert_equal ([ 0., 0. ] -$ [ -.inf, -1.; 1., inf ]) [ -.inf, -1.; 1., inf ]) *)
       ; ("multiplication"
         >:: fun _ ->ignore 0)
         (* assert_equal ([ -1., 1. ] *$ [ inf, inf ]) [ -.inf, -.inf; inf, inf ] *)
         (* assert_equal ([ 1., 1. ] /$ [ -0., 0. ]) [];
         assert_equal ([ 0., 0. ] /$ [ -0., 0. ]) [] *)
         ) *)
       ]
;;

let () = run_test_tt_main suite

(* let () =
  let special = [ 0., 0.; 0., 1.; 0., infinity; 1., 1.; 1., infinity ] in
  
    ignore @@ map_cartesian_product
    (fun a b ->
      Printf.printf
        "%10s * %10s = %s\n"
        (Set.to_string [a])
        (Set.to_string [b])
        (Set.to_string ([a] *$ [b]))
    special
    special
;; *)