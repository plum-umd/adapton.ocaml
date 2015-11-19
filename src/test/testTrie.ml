open Trie

let padr n s =
  let slen = String.length s in
  if slen < n then
    s ^ String.make (n - String.length s + 1) ' '
  else s
    
let padl n s =
  let slen = String.length s in
  if slen < n then
    String.make (n - String.length s + 1) ' ' ^ s
  else s

type test = unit -> string

let unary_tests (type i) (type o)
    ~name ~show_in ~show_out ?(eq=(=))
    (f : i -> o) (l : (i * o) list)
  : test list =
  let nm = padr 10 name in
  List.mapi
    (fun n (i, o) () ->
       let sn = padl 4 (string_of_int (n+1)) in
       try  let f_i = f i in
            if eq f_i o
            then (Printf.printf ".%!" ; Printf.sprintf "%s | %s | SUCCESS" sn nm)
            else (Printf.printf "F%!" ;
                  Printf.sprintf "%s | %s | FAILURE:\nexpected:\n(%s %s) = %s,\nbut got:\n%s."
                    sn nm name (show_in i) (show_out o) (show_out f_i))
       with exc ->
            Printf.printf "E%!" ;
            Printf.sprintf "%s | %s | ERROR:\nexpected:\n(%s %s) = %s,\nbut caught this error:\n%s."
              sn nm name (show_in i) (show_out o) (Printexc.to_string exc))
    l

let binary_tests (type i) (type j) (type o)
    ~name ~show_in ~show_out ?(eq=(=))
    (f : i -> j -> o) (l : (i * j * o) list)
  : test list =
  let nm = padr 10 name in
  List.mapi
    (fun n (i, j, o) () ->
       let sn = padl 4 (string_of_int (n+1)) in
       try  let f_i_j = f i j in
            if eq f_i_j o
            then (Printf.printf ".%!" ; Printf.sprintf "%s | %s | SUCCESS" sn nm)
            else (Printf.printf "F%!" ;
                  Printf.sprintf "%s | %s | FAILURE:\nexpected:\n(%s %s) = %s,\nbut got:\n%s."
                    sn nm name (show_in i j) (show_out o) (show_out f_i_j))
       with exc ->
            Printf.printf "E%!" ;
            Printf.sprintf "%s | %s | ERROR:\nexpected:\n(%s %s) = %s,\nbut caught this error:\n%s."
              sn nm name (show_in i j) (show_out o) (Printexc.to_string exc))
    l

let ternary_tests (type i) (type j) (type k) (type o)
    ~name ~show_in ~show_out ?(eq=(=))
    (f : i -> j -> k -> o) (l : (i * j * k * o) list)
  : test list =
  let nm = padr 10 name in
  List.mapi
    (fun n (i, j, k, o) () ->
       let sn = padl 4 (string_of_int (n+1)) in
       try  let f_i_j_k = f i j k in
            if eq f_i_j_k o
            then (Printf.printf ".%!" ; Printf.sprintf "%s | %s | SUCCESS" sn nm)
            else (Printf.printf "F%!" ;
                  Printf.sprintf "%s | %s | FAILURE:\nexpected:\n(%s %s) = %s,\nbut got:\n%s."
                    sn nm name (show_in i j k) (show_out o) (show_out f_i_j_k))
       with exc ->
            Printf.printf "E%!" ;
            Printf.sprintf "%s | %s | ERROR:\nexpected:\n(%s %s) = %s,\nbut caught this error:\n%s."
              sn nm name (show_in i j k) (show_out o) (Printexc.to_string exc))
    l

let nm = Name.gensym

let bs_suite =
  let pow_tests  =
    binary_tests ~name:"pow"
      ~show_in:(Printf.sprintf "%i %i")
      ~show_out:string_of_int
      BS.pow
      [0, 0, 1;
       42, 0, 1;
       ~-2, 0, 1;
       2, 4, 16;
       ~-2, 3, ~-8] in
  let flip_tests =
    binary_tests ~name:"flip"
      ~show_in:(Printf.sprintf "%i %i")
      ~show_out:string_of_int
      BS.flip
      [0, 0, 1;
       2, 0, 4;
       1, 7, 5;
       3, 7, 15] in
  let is_set_tests =
    binary_tests ~name:"is_set"
      ~show_in:(Printf.sprintf "%i %i")
      ~show_out:string_of_bool
      BS.is_set
      [4, 0, false;
       0, 0, false;
       0, 1, true;
       1, 2, true;
       0, 2, false] in
  let prepend_tests =
    binary_tests
      ~name:"prepend"
      ~show_in:(fun i bs -> Printf.sprintf "%i %s" i (BS.show bs))
      ~show_out:BS.show
      BS.prepend
      [0, (0, 0), (1, 0);
       1, (0, 0), (1, 1);
       1, (1, 1), (2, 3);
       0, (1, 1), (2, 1);
       1, (2, 1), (3, 5)] in
  "bit strings", pow_tests@flip_tests@is_set_tests@prepend_tests

module NIS = Set.MakeNonInc(Name)(Insts.Nominal)(Types.String)
module IS = Set.Make(Name)(Insts.FromScratch)(Types.String)

let set_suite =
  let min_depth = 4 in
  let e001 = "a" in
  let e010 = "d" in
  let e010'= "j" in
  let e010''= "gah" in
  let t0   = NIS.empty ~min_depth in
  let t1   = NIS.add t0  e001 in
  let t2   = NIS.add t1  e010 in
  let t3   = NIS.add t2  e010 in
  let t4   = NIS.add t3  e010' in
  let t1'  = NIS.add t0  e001 in
  let t2'  = NIS.add t1' e010 in
  let t3'  = NIS.add t2' e010 in
  let t4'  = NIS.add t3' e010' in
  let t5'  = NIS.add t4' e010' in
  let t6'  = NIS.add t5' e010' in
  let cardinal_tests = unary_tests ~name:"cardinal"
      ~show_in:NIS.show
      ~show_out:string_of_int
      NIS.cardinal
      [t0, 0; t1, 1; t2, 2; t1', 1; t2', 2; t3, 2; t3', 2; t4, 3; t4', 3] in
  let mem_tests = binary_tests ~name:"mem"
      ~show_in:(fun t e -> Printf.sprintf "%s %s" (NIS.show t) e)
      ~show_out:string_of_bool
      NIS.mem
      [t0, e001, false; t0, e010, false;
       t1, e001, true; t1, e010, false;
       t2, e001, true; t2, e010, true; t2, e010', false; t2, e010'', false;
       t3, e001, true; t3, e010, true; t3, e010', false; t3, e010'', false;
       t4, e001, true; t4, e010, true; t4, e010', true; t4', e010'', false;
       t5', e010', true; t6', e010', true] in
  let equal_tests = binary_tests ~name:"equal"
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (NIS.show t) (NIS.show t'))
      ~show_out:string_of_bool
      NIS.equal
      [t0, t0, true;
       t0, t1, false;
       t1, t1, true;
       t1, t1', true;
       t2, t2', true;
       t4', t4', true;
       t5', t4', true;
       t6', t5', true
      ] in
  let hash_eq_tests = binary_tests ~name:"hasheq"
      ~show_out:string_of_bool
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (NIS.show t) (NIS.show t'))
      (fun x y -> let h = NIS.hash 42 in h x = h y)
      [t0, t0, true;
       t1, t1', true;
       t2, t2', true;
       t1', t2, false;
       t1', t2, false;
      ] in
  let string_of_list sep l =
    let elts =
      if List.length l > 0 then
        let elts = List.fold_right (fun elt a -> elt^sep^a) l "" in
        (String.sub elts 0 ((String.length elts)-(String.length sep)))
      else ""
    in
    "[" ^ elts ^ "]"
  in
  let of_list_tests = unary_tests ~name:"of_list"
      ~show_in:(string_of_list ";")
      ~show_out:NIS.show
      ~eq:NIS.equal
      (NIS.of_list ~min_depth)
      [["1"; "2"; "3"], NIS.add (NIS.add (NIS.add t0 "1") "2") "3";
       ["1"; "2"; "3"], NIS.add (NIS.add (NIS.add t0 "3") "2") "1";
       ["3"; "2"; "1"], NIS.add (NIS.add (NIS.add t0 "1") "2") "3";
       ["2"], NIS.add t0 "2";
       ["1"], NIS.add t0 "1"] in
  let union_tests = binary_tests ~name:"union"
      ~show_out:NIS.show
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (NIS.show t) (NIS.show t'))
      ~eq:NIS.equal
      NIS.union
      [NIS.of_list ~min_depth ["1"; "2"; "3"], t0, NIS.of_list ~min_depth ["1"; "2"; "3"];
       t0, NIS.of_list ~min_depth ["1"; "2"; "3"], NIS.of_list ~min_depth ["1"; "2"; "3"];
       NIS.of_list ~min_depth:1 ["1"; "2"; "3"], NIS.singleton ~min_depth:1 "4",
       NIS.of_list ~min_depth:1 ["1"; "2"; "3"; "4"];
       NIS.of_list ~min_depth ["1"; "2"; "3"], NIS.singleton ~min_depth "4",
       NIS.of_list ~min_depth ["1"; "2"; "3"; "4"];
       NIS.of_list ~min_depth:5 ["1"; "2"; "3"], NIS.singleton ~min_depth:5 "4",
       NIS.of_list ~min_depth:5 ["1"; "2"; "3"; "4"];
       NIS.of_list ["one"; "42"; "five-hundred"], NIS.of_list ["1"; "forty-two"; "500"],
       NIS.of_list ["one"; "42"; "five-hundred"; "1"; "forty-two"; "500"];
       NIS.of_list ~min_depth:3 ["1"; "forty-two"; "five-hundred"],
       NIS.of_list ~min_depth:3 ["1"; "forty-two"; "500"],
       NIS.of_list ~min_depth:3 ["five-hundred"; "1"; "forty-two"; "500"]] in
  "Set", (cardinal_tests@mem_tests@of_list_tests@union_tests@equal_tests@hash_eq_tests)

let nset_suite ~art_ifreq ~min_depth =
  let e001 = "a" in
  let e010 = "d" in
  let e010'= "j" in
  let e010''= "gah" in

  let singleton = IS.singleton ~art_ifreq ~min_depth in
  let of_list = IS.of_list ~art_ifreq ~min_depth in
  
  let t0   = IS.empty ~art_ifreq ~min_depth in

  let u1 = IS.add (nm()) t0 e001 in
  let _ = IS.add (nm()) u1 e010 in
  let _ = IS.add (nm()) t0 e010' in
  let _ = IS.add (nm()) t0 e010'' in


  let t1   = IS.add (nm()) t0  e001 in
  let t2   = IS.add (nm()) t1  e010 in
  let t3   = IS.add (nm()) t2  e010 in
  let t4   = IS.add (nm()) t3  e010' in
  let t1'  = IS.add (nm()) t0  e001 in
  let t2'  = IS.add (nm()) t1' e010 in
  let t3'  = IS.add (nm()) t2' e010 in
  let t4'  = IS.add (nm()) t3' e010' in
  let t5'  = IS.add (nm()) t4' e010' in
  let t6'  = IS.add (nm()) t5' e010' in
  let union_tests =
    ternary_tests
      ~show_out:IS.show
      ~show_in:(fun nm t t' -> Printf.sprintf "%s %s %s" (Name.show nm) (IS.show (IS.force t)) (IS.show (IS.force t')))
      ~eq:(fun a b -> IS.equal (IS.force a) (IS.force b))
      ~name:"nunion"
      IS.union
      [(nm()), of_list (nm()) ["1"; "2"; "3"], t0, of_list (nm())["1"; "2"; "3"];
       (nm()), t0, of_list (nm())["1"; "2"; "3"], of_list (nm())["1"; "2"; "3"];
       (nm()), of_list (nm()) ["1"; "2"; "3"], singleton (nm()) "4",
       of_list (nm()) ["1"; "2"; "3"; "4"];
       (nm ()), of_list (nm()) ["1"; "2"; "3"], singleton (nm()) "4",
       of_list (nm())["1"; "2"; "3"; "4"];
       (nm ()), of_list (nm()) ["1"; "2"; "3"], singleton (nm()) "4",
       of_list (nm()) ["1"; "2"; "3"; "4"];
       (nm ()), of_list (nm()) ["one"; "42"; "five-hundred"], of_list (nm()) ["1"; "forty-two"; "500"],
       of_list (nm()) ["one"; "42"; "five-hundred"; "1"; "forty-two"; "500"];
       (nm ()), of_list (nm()) ["1"; "forty-two"; "five-hundred"],
       of_list (nm()) ["1"; "forty-two"; "500"],
       of_list (nm()) ["five-hundred"; "1"; "forty-two"; "500"]] in
  let cardinal_tests = unary_tests ~name:"cardinal"
      ~show_in:(fun t -> Printf.sprintf "%s" (IS.show (IS.force t)))
      ~show_out:string_of_int
      IS.cardinal
      [t0, 0; t1, 1; t2, 2; t1', 1; t2', 2; t3, 2; t3', 2; t4, 3; t4', 3] in
  let mem_tests = binary_tests ~name:"mem"
      ~show_in:(fun t -> Printf.sprintf "%s %s" (IS.show (IS.force t)))
      ~show_out:string_of_bool
      IS.mem
      [t0, e001, false; t0, e010, false;
       t1, e001, true; t1, e010, false;
       t2, e001, true; t2, e010, true; t2, e010', false; t2, e010'', false;
       t3, e001, true; t3, e010, true; t3, e010', false; t3, e010'', false;
       t4, e001, true; t4, e010, true; t4, e010', true; t4', e010'', false;
       t5', e010', true; t6', e010', true] in
  let equal_tests = binary_tests ~name:"equal"
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (IS.show (IS.force t)) (IS.show (IS.force t')))
      ~show_out:string_of_bool
      IS.equal
      [t0, t0, true;
       t0, t1, false;
       t1, t1, true;
       t1, t1', false;
       t2, t2', false;
       t4', t4', true;
       t5', t4', false;
       t6', t5', false
      ] in
  let hash_eq_tests = binary_tests ~name:"hasheq"
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (IS.show (IS.force t)) (IS.show (IS.force t')))
      ~show_out:string_of_bool
      (fun x y -> let h = IS.hash 42 in h x = h y)
      [t0, t0, true;
       t1, t1', false;
       t2, t2', false;
       t1', t2, false;
       t1', t2, false;
      ] in
  "Nominal Set", (cardinal_tests@mem_tests@equal_tests@hash_eq_tests@union_tests)

module NIM = Map.MakeNonInc(Name)(Insts.FromScratch)(Types.String)(Types.Int)
module IM = Map.Make(Name)(Insts.Nominal)(Types.String)(Types.Int)

let map_suite ~min_depth = 
  let k0, k1, k2, k3, k4     = "a", "c", "d", "j", "gah"   in
  let v0, v1, v2, v3, v4, v5 =  1,   3,   4,   5,    6,  7 in
  let nt0 = NIM.empty ~min_depth in
  let nt1, nt1' = NIM.add nt0 k0 v0, NIM.add nt0  k1 v1 in
  let nt2, nt2' = NIM.add nt1 k1 v1, NIM.add nt1' k0 v0 in
  let nt3, nt3' = NIM.add nt2 k2 v2, NIM.add nt2' k2 v2 in
  let nt4, nt4' = NIM.add nt3 k3 v3, NIM.add nt3' k3 v3 in
  let nt5, nt5' = NIM.add nt4 k0 v5, NIM.add nt4' k0 v5 in
  let ncardinal_tests = unary_tests ~name:"cardinal"
      ~show_in:(fun t -> NIM.show (NIM.force t))
      ~show_out:string_of_int
      NIM.cardinal
      [nt0, 0; nt1, 1; nt1', 1; nt2, 2; nt2', 2; nt3, 3; nt3', 3; nt4, 4; nt4', 4; nt5, 4] in
  let nmem_tests = binary_tests ~name:"mem"
      ~show_in:(fun t -> Printf.sprintf "%s %s" (NIM.show (NIM.force t)))
      ~show_out:string_of_bool
      NIM.mem
      [nt0, k0, false; nt0, k1, false;
       NIM.add nt1 k0 v1, k0, true;
       nt1, k0, true; nt1', k0, false; nt1', k1, true;
       nt2, k1, true; nt2', k1, true; nt2', k0, true;
       nt3, k0, true; nt3', k0, true; nt3, k2, true;
       nt3', k2, true; nt3, k3, false; nt3', k3, false]
  in
  let nfind_tests = binary_tests
      ~name:"find"
      ~show_in:(fun t -> Printf.sprintf "%s %s" (NIM.show (NIM.force t)))
      ~show_out:(function Some v -> string_of_int v | None -> "*not found*")
      NIM.find
      [nt1, k0, Some v0; nt1', k1, Some v1;
       nt2, k0, Some v0; nt2', k1, Some v1;
       nt2, k1, Some v1; nt2', k0, Some v0;
       nt3, k0, Some v0; nt3', k1, Some v1;
       nt3, k1, Some v1; nt3', k0, Some v0;
       nt3, k2, Some v2; nt3', k2, Some v2;
       nt5, k0, Some v5; nt5', k1, Some v1;
       nt5, k1, Some v1; nt5', k0, Some v5;
       nt5, k2, Some v2; nt5', k2, Some v2;
       NIM.add nt1 k0 v1, k0, Some v1;
       NIM.add (NIM.add nt1 k0 v1) k0 v2, k0, Some v2;
      ]
  in
  let nequal_tests = binary_tests ~name:"equal"
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (NIM.show (NIM.force t)) (NIM.show (NIM.force t')))
      ~show_out:string_of_bool NIM.equal
      [(NIM.force nt5), (NIM.force nt4), false]
  in
  "Trie Map", (ncardinal_tests@nmem_tests@nfind_tests@nequal_tests)

let nmap_suite ~art_ifreq ~min_depth = 
  let k0, k1, k2, k3, k4     = "a", "c", "d", "j", "gah"   in
  let v0, v1, v2, v3, v4, v5 =  1,   3,   4,   5,    6,  7 in
  let nt0 = IM.empty ~art_ifreq ~min_depth in
  let nt1, nt1' = IM.add (nm()) nt0 k0 v0, IM.add (nm()) nt0  k1 v1 in
  let nt2, nt2' = IM.add (nm()) nt1 k1 v1, IM.add (nm()) nt1' k0 v0 in
  let nt3, nt3' = IM.add (nm()) nt2 k2 v2, IM.add (nm()) nt2' k2 v2 in
  let nt4, nt4' = IM.add (nm()) nt3 k3 v3, IM.add (nm()) nt3' k3 v3 in
  let nt5, nt5' = IM.add (nm()) nt4 k0 v5, IM.add (nm()) nt4' k0 v5 in
  let ncardinal_tests = unary_tests ~name:"cardinal"
      ~show_in:(fun t -> IM.show (IM.force t))
      ~show_out:string_of_int
      IM.cardinal
      [nt0, 0; nt1, 1; nt1', 1; nt2, 2; nt2', 2; nt3, 3; nt3', 3; nt4, 4; nt4', 4; nt5, 4] in
  let nmem_tests = binary_tests ~name:"mem"
      ~show_in:(fun t -> Printf.sprintf "%s %s" (IM.show (IM.force t)))
      ~show_out:string_of_bool
      IM.mem
      [nt0, k0, false; nt0, k1, false;
       IM.add (nm()) nt1 k0 v1, k0, true;
       nt1, k0, true; nt1', k0, false; nt1', k1, true;
       nt2, k1, true; nt2', k1, true; nt2', k0, true;
       nt3, k0, true; nt3', k0, true; nt3, k2, true;
       nt3', k2, true; nt3, k3, false; nt3', k3, false]
  in
  let nfind_tests = binary_tests
      ~name:"find"
      ~show_in:(fun t -> Printf.sprintf "%s %s" (IM.show (IM.force t)))
      ~show_out:(function Some v -> string_of_int v | None -> "*not found*")
      IM.find
      [nt1, k0, Some v0; nt1', k1, Some v1;
       nt2, k0, Some v0; nt2', k1, Some v1;
       nt2, k1, Some v1; nt2', k0, Some v0;
       nt3, k0, Some v0; nt3', k1, Some v1;
       nt3, k1, Some v1; nt3', k0, Some v0;
       nt3, k2, Some v2; nt3', k2, Some v2;
       nt5, k0, Some v5; nt5', k1, Some v1;
       nt5, k1, Some v1; nt5', k0, Some v5;
       nt5, k2, Some v2; nt5', k2, Some v2;
       IM.add (nm ()) nt1 k0 v1, k0, Some v1;
       IM.add (nm ()) (IM.add (nm ()) nt1 k0 v1) k0 v2, k0, Some v2;
      ]
  in
  let nequal_tests = binary_tests ~name:"equal"
      ~show_in:(fun t t' -> Printf.sprintf "%s %s" (IM.show (IM.force t)) (IM.show (IM.force t')))
      ~show_out:string_of_bool IM.equal
      [(IM.force nt5), (IM.force nt4), false]
  in
  "Nominal Map", (ncardinal_tests@nmem_tests@nfind_tests@nequal_tests)

let run_suite (nm, tests) =
  Printf.printf "%s suite %!" nm ;
  let results = List.map (fun test -> test ()) tests in
  Printf.printf "\n%!" ;
  List.iter (Printf.printf "%s\n%!") results

let run () =
  let all_pairs xs ys =
    List.fold_left
      (fun a x -> List.fold_left (fun a y -> (x, y)::a) a ys)
      []
      xs
  in
  let fold_down ?(step=(fun n -> n+1)) f a n =
    let rec loop acc = function
      | n when n > 0 -> let n' = n-1 in loop (f acc n') n'
      | _ -> acc
    in
    loop a (n-1)
  in
  let freqs =
    (fold_down (fun a i -> (`Const ((i+1) * (i+1)))::a) [] 4)
  @ (fold_down (fun a i -> (`First ((i+1) * (i+1)))::a) [] 4)
  @ (fold_down
       (fun a i -> fold_down (fun a j -> (`Depth (i+2, j+1))::a) a 3)
       []
       3)
  in
  let mds = fold_down (fun a n -> (n+1)::a) [] 8 in
  run_suite bs_suite;
  run_suite set_suite;
  List.iter
    (fun min_depth -> run_suite (map_suite ~min_depth))
    mds ;
  List.iter
    (fun (art_ifreq, min_depth) ->
       run_suite (nset_suite ~art_ifreq ~min_depth))
    (all_pairs freqs mds) ;
  List.iter
    (fun (art_ifreq, min_depth) ->
       run_suite (nmap_suite ~art_ifreq ~min_depth))
    (all_pairs freqs mds) ;
  ()

let _ = run ()
