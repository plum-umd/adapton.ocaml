open OUnit2
open Trie

let unary_tests (type i) (type o)
    ?(printer=(fun _ -> "no printer"))
    ?(eq=(=))
    (nm : string)
    (f : i -> o)
    (l : (i * o) list)
  : test list =
  List.map
    (fun (i, o) ->
       nm >:: (fun ctxt -> assert_equal ~printer ~cmp:eq ~ctxt (f i) o))
    l

let binary_tests (type i) (type j) (type o)
    ?(printer=(fun _ -> "no printer"))
    ?(eq=(=))
    (nm : string)
    (f : i -> j -> o)
    (l : (i * j * o) list)
  : test list =
  List.map
    (fun (i0, i1, o) ->
       nm >:: (fun ctxt -> assert_equal ~printer ~cmp:eq ~ctxt o (f i0 i1)))
    l

let nm = Name.gensym

let bs_suite =
  let pow_tests  =
    binary_tests "pow" BS.pow
      [0, 0, 1;
       42, 0, 1;
       ~-2, 0, 1;
       2, 4, 16;
       ~-2, 3, ~-8] in
  let flip_tests =
    binary_tests "flip" BS.flip
      [0, 0, 1;
       2, 0, 4;
       1, 7, 5;
       3, 7, 15] in
  let is_set_tests =
    binary_tests "is_set" BS.is_set
      [4, 0, false;
       0, 0, false;
       0, 1, true;
       1, 2, true;
       0, 2, false] in
  let prepend_tests =
    binary_tests "prepend" BS.prepend
      [0, (0, 0), (1, 0);
       1, (0, 0), (1, 1);
       1, (1, 1), (2, 3);
       0, (1, 1), (2, 1);
       1, (2, 1), (3, 5)] in
  "bit strings" >::: pow_tests@flip_tests@is_set_tests@prepend_tests

module S = Set.Make(Name)(Insts.Nominal)(Types.String)

let set_suite =
  let min_depth = 4 in
  let e001 = "a" in
  let e010 = "d" in
  let e010'= "j" in
  let e010''= "gah" in
  let t0   = S.empty ~min_depth in
  let t1   = S.add t0  e001 in
  let t2   = S.add t1  e010 in
  let t3   = S.add t2  e010 in
  let t4   = S.add t3  e010' in
  let t1'  = S.add t0  e001 in
  let t2'  = S.add t1' e010 in
  let t3'  = S.add t2' e010 in
  let t4'  = S.add t3' e010' in
  let t5'  = S.add t4' e010' in
  let t6'  = S.add t5' e010' in
  let cardinal_tests = unary_tests "cardinal" S.cardinal
      [t0, 0; t1, 1; t2, 2; t1', 1; t2', 2; t3, 2; t3', 2; t4, 3; t4', 3] in
  let mem_tests = binary_tests "mem" S.mem
      [t0, e001, false; t0, e010, false;
       t1, e001, true; t1, e010, false;
       t2, e001, true; t2, e010, true; t2, e010', false; t2, e010'', false;
       t3, e001, true; t3, e010, true; t3, e010', false; t3, e010'', false;
       t4, e001, true; t4, e010, true; t4, e010', true; t4', e010'', false;
       t5', e010', true; t6', e010', true] in
  let equal_tests = binary_tests "equal" S.equal
      [t0, t0, true;
       t0, t1, false;
       t1, t1, true;
       t1, t1', true;
       t2, t2', true;
       t4', t4', true;
       t5', t4', true;
       t6', t5', true
      ] in
  let hash_eq_tests = binary_tests "hasheq" (fun x y -> let h = S.hash 42 in h x = h y)
      [t0, t0, true;
       t1, t1', true;
       t2, t2', true;
       t1', t2, false;
       t1', t2, false;
      ] in
  let of_list_tests = unary_tests ~printer:S.show ~eq:S.equal "of_list" (S.of_list ~min_depth)
      [["1"; "2"; "3"], S.add (S.add (S.add t0 "1") "2") "3";
       ["1"; "2"; "3"], S.add (S.add (S.add t0 "3") "2") "1";
       ["3"; "2"; "1"], S.add (S.add (S.add t0 "1") "2") "3";
       ["2"], S.add t0 "2";
       ["1"], S.add t0 "1"] in
  let union_tests = binary_tests ~printer:S.show ~eq:S.equal "union" S.union
      [S.of_list ~min_depth ["1"; "2"; "3"], t0, S.of_list ~min_depth ["1"; "2"; "3"];
       t0, S.of_list ~min_depth ["1"; "2"; "3"], S.of_list ~min_depth ["1"; "2"; "3"];
       (* This is not true for any min depth < 4.
          S.of_list ~min_depth:3 ["1"; "2"; "3"], S.singleton ~min_depth:3 "4",
          S.of_list ~min_depth:3 ["1"; "2"; "3"; "4"]; *)
       S.of_list ~min_depth ["1"; "2"; "3"], S.singleton ~min_depth "4",
       S.of_list ~min_depth ["1"; "2"; "3"; "4"];
       S.of_list ~min_depth:5 ["1"; "2"; "3"], S.singleton ~min_depth:5 "4",
       S.of_list ~min_depth:5 ["1"; "2"; "3"; "4"];
       S.of_list ["one"; "42"; "five-hundred"], S.of_list ["1"; "forty-two"; "500"],
       S.of_list ["one"; "42"; "five-hundred"; "1"; "forty-two"; "500"];
       S.of_list ~min_depth:3 ["1"; "forty-two"; "five-hundred"],
       S.of_list ~min_depth:3 ["1"; "forty-two"; "500"],
       S.of_list ~min_depth:3 ["five-hundred"; "1"; "forty-two"; "500"]] in
  "Set" >::: (cardinal_tests@mem_tests@of_list_tests@union_tests@equal_tests@hash_eq_tests)

let nset_suite =
  let min_depth = 4 in
  let e001 = "a" in
  let e010 = "d" in
  let e010'= "j" in
  let e010''= "gah" in
  let t0   = S.empty ~min_depth in

  let u1 = S.nadd (nm()) t0 e001 in
  let _ = S.nadd (nm()) u1 e010 in
  let _ = S.nadd (nm()) t0 e010' in
  let _ = S.nadd (nm()) t0 e010'' in


  let t1   = S.nadd (nm()) t0  e001 in
  let t2   = S.nadd (nm()) t1  e010 in
  let t3   = S.nadd (nm()) t2  e010 in
  let t4   = S.nadd (nm()) t3  e010' in
  let t1'  = S.nadd (nm()) t0  e001 in
  let t2'  = S.nadd (nm()) t1' e010 in
  let t3'  = S.nadd (nm()) t2' e010 in
  let t4'  = S.nadd (nm()) t3' e010' in
  let t5'  = S.nadd (nm()) t4' e010' in
  let t6'  = S.nadd (nm()) t5' e010' in
  let union_tests = binary_tests ~printer:S.show ~eq:(fun a b -> S.equal (S.force a) (S.force b)) "union" S.union
      [S.of_list ~min_depth ["1"; "2"; "3"], t0, S.of_list ~min_depth ["1"; "2"; "3"];
       t0, S.of_list ~min_depth ["1"; "2"; "3"], S.of_list ~min_depth ["1"; "2"; "3"];
       (* This is not true for any min depth < 4.
          S.of_list ~min_depth (nm()):3 ["1"; "2"; "3"], S.singleton ~min_depth:3 "4",
          S.of_list ~min_depth (nm()):3 ["1"; "2"; "3"; "4"]; *)
       S.of_list ~min_depth ["1"; "2"; "3"], S.singleton ~min_depth "4",
       S.of_list ~min_depth ["1"; "2"; "3"; "4"];
       S.of_list ~min_depth:5 ["1"; "2"; "3"], S.singleton ~min_depth:5 "4",
       S.of_list ~min_depth:5 ["1"; "2"; "3"; "4"];
       S.of_list ["one"; "42"; "five-hundred"], S.of_list ["1"; "forty-two"; "500"],
       S.of_list ["one"; "42"; "five-hundred"; "1"; "forty-two"; "500"];
       S.of_list ~min_depth:3 ["1"; "forty-two"; "five-hundred"],
       S.of_list ~min_depth:3 ["1"; "forty-two"; "500"],
       S.of_list ~min_depth:3 ["five-hundred"; "1"; "forty-two"; "500"]] in
  let cardinal_tests = unary_tests "cardinal" S.cardinal
      [t0, 0; t1, 1; t2, 2; t1', 1; t2', 2; t3, 2; t3', 2; t4, 3; t4', 3] in
  let mem_tests = binary_tests "mem" S.mem
      [t0, e001, false; t0, e010, false;
       t1, e001, true; t1, e010, false;
       t2, e001, true; t2, e010, true; t2, e010', false; t2, e010'', false;
       t3, e001, true; t3, e010, true; t3, e010', false; t3, e010'', false;
       t4, e001, true; t4, e010, true; t4, e010', true; t4', e010'', false;
       t5', e010', true; t6', e010', true] in
  let equal_tests = binary_tests "equal" S.equal
      [t0, t0, true;
       t0, t1, false;
       t1, t1, true;
       t1, t1', false;
       t2, t2', false;
       t4', t4', true;
       t5', t4', false;
       t6', t5', false
      ] in
  let hash_eq_tests = binary_tests "hasheq" (fun x y -> let h = S.hash 42 in h x = h y)
      [t0, t0, true;
       t1, t1', false;
       t2, t2', false;
       t1', t2, false;
       t1', t2, false;
      ] in
  "Nominal Set" >::: (cardinal_tests@mem_tests@equal_tests@hash_eq_tests@union_tests)

module M = Map.Make(Name)(Insts.Nominal)(Types.String)(Types.Int)

let nmap_suite = 
  let k0, k1, k2, k3, k4     = "a", "c", "d", "j", "gah"   in
  let v0, v1, v2, v3, v4, v5 =  1,   3,   4,   5,    6,  7 in
  let nt0 = M.empty ~min_depth:1 in
  let nt1, nt1' = M.nadd (nm()) nt0 k0 v0, M.nadd (nm()) nt0  k1 v1 in
  let nt2, nt2' = M.nadd (nm()) nt1 k1 v1, M.nadd (nm()) nt1' k0 v0 in
  let nt3, nt3' = M.nadd (nm()) nt2 k2 v2, M.nadd (nm()) nt2' k2 v2 in
  let nt4, nt4' = M.nadd (nm()) nt3 k3 v3, M.nadd (nm()) nt3' k3 v3 in
  let nt5, nt5' = M.nadd (nm()) nt4 k0 v5, M.nadd (nm()) nt4' k0 v5 in
  let ncardinal_tests = unary_tests "cardinal" M.cardinal
      [(*nt0, 0; nt1, 1; nt1', 1; nt2, 2; nt2', 2; nt3, 3; nt3', 3; nt4, 4; nt4', 4; nt5, 4*)] in
  let nmem_tests = binary_tests "mem" M.mem
      [nt0, k0, false; nt0, k1, false;
       M.nadd (Name.gensym ()) nt1 k0 v1, k0, true;
       nt1, k0, true; nt1', k0, false; nt1', k1, true;
       nt2, k1, true; nt2', k1, true; nt2', k0, true;
       nt3, k0, true; nt3', k0, true; nt3, k2, true;
       nt3', k2, true; nt3, k3, false; nt3', k3, false] in
  let nfind_tests = binary_tests "find" M.find
      [nt1, k0, Some v0; nt1', k1, Some v1;
       nt2, k0, Some v0; nt2', k1, Some v1;
       nt2, k1, Some v1; nt2', k0, Some v0;
       nt3, k0, Some v0; nt3', k1, Some v1;
       nt3, k1, Some v1; nt3', k0, Some v0;
       nt3, k2, Some v2; nt3', k2, Some v2;
       nt5, k0, Some v5; nt5', k1, Some v1;
       nt5, k1, Some v1; nt5', k0, Some v5;
       nt5, k2, Some v2; nt5', k2, Some v2;] in
  let nequal_tests = binary_tests "equal" M.equal
      [(M.force nt5), (M.force nt4), false] in
  print_endline (M.show (M.force nt5)) ;
  print_endline (M.show (M.force nt4)) ;
  "Nominal Map" >::: (ncardinal_tests@nmem_tests@nfind_tests@nequal_tests)

let run () =
  run_test_tt_main bs_suite;
  run_test_tt_main nset_suite;
  run_test_tt_main nmap_suite;
  ()

let _ = run ()
