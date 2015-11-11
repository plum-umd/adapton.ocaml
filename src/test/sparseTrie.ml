let min_depth : int ref              = ref 1
let art_ifreq : Trie.Meta.Freq.t ref = ref (`Const 1)
let size      : int ref              = ref 10
let data_type : string ref           = ref "ocaml-set"
let verbose   : bool ref             = ref false

module Set =
struct
  let nm = Name.gensym

  module type TSET =
  sig
    type t
    type elt = int
    val mt : t
    val add : t -> elt -> t
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val to_list : t -> elt list
    val name : string
  end
  
  module NTrie : TSET =
  struct
    module S = Trie.Set.MakeInc(Name)(Insts.Nominal)(Types.Int)
    type t = S.t
    type elt = int
    let mt = S.empty ~art_ifreq:!art_ifreq ~min_depth:!min_depth (nm())
    let add t e = S.add (nm()) t e
    let fold = S.fold
    let to_list = S.to_list
    let name = "ntrie-set"
  end
  module FSTrie : TSET =
  struct
    module S = Trie.Set.MakeNonInc(Name)(Insts.FromScratch)(Types.Int)
    type t = S.t
    type elt = int
    let mt = S.empty ~min_depth:!min_depth
    let add = S.add
    let fold = S.fold
    let to_list = S.to_list
    let name = "trie-set"
  end
  module OCaml : TSET =
  struct
    module S = Set.Make(Types.Int)
    type t = S.t
    type elt = int
    let mt = S.empty
    let add t e = S.add e t
    let fold f a t = S.fold (fun n a -> f a n) t a
    let to_list t = S.fold (fun n a -> n::a) t []
    let name = "ocaml-set"
  end

  module Test(S : TSET) =
  struct

    let fill =
      let check = !size / 10 in
      let rec loop acc = function
        | n when n > 0 ->
          (if !verbose && n mod check = 0 then
             Printf.printf "filled %i.\n%!" (!size-n)) ;
          loop (S.add acc n) (n-1)
        | n -> acc
      in
      loop S.mt

    module OS = Set.Make(Types.Int)

    let test () =
      (if !verbose then
         Printf.printf "Filling %s (md=%i, af=%s) set with %i elements.\n%!"
           S.name !min_depth (Trie.Meta.Freq.show !art_ifreq) !size) ;
      let s = fill !size  in
      let os = S.fold (fun a n -> OS.add n a) OS.empty s in
      let cos = OS.cardinal os in
      (if !verbose then
         Printf.printf "%s : found %i / %i elements\n%!"
           (if cos = !size then "SUCCESS" else "FAILURE")
           cos !size)
      (* ignore (S.fold (fun _ n -> Printf.printf "found: %i\n%!" n) () s) *)
      (* Printf.printf "Converting to list to check size.\n%!" ;
      let l = S.to_list s in
      if List.length l = !size
      then print_endline " success!"
      else print_endline " failure!"*)

  end

  let run () =
    let ocamlset = (module OCaml : TSET) in
    let insts =
      [ NTrie.name,  (module  NTrie : TSET)
      ; FSTrie.name, (module FSTrie : TSET)
      ]
    in
    let module S =
      (val
        (List.fold_left
           (fun a (nm, s) -> if nm = !data_type then s else a)
           ocamlset
           insts))
    in
    let module T = Test (S) in
    T.test ()

end

let main () =
  let cmd_name = Sys.argv.(0) in
  let usage =
    Printf.sprintf
      "Usage: %s [-size pos-int] [-min-depth pos-int] [-art-ifreq (depth | pos-int)] [-type (trie set | ntrie set | ocaml set)]\n"
      cmd_name
  in
  let handle_ifreq s =
    let ifreq =
      try
        let len = String.length s in
        if len > 0 && s.[0] = 'd' then
          `Depth (int_of_string (String.sub s 1 (len-1)))
        else `Const (int_of_string s)
      with _ ->
        failwith ("Bad art_ifreq, expected positive int or \"depth\", but got " ^ s)
    in
    art_ifreq := ifreq
  in
  let _ =
    Arg.parse
      (Arg.align
         [ ("-size", Arg.Set_int size, "size of the set")
         ; ("-type", Arg.Set_string data_type, "data structure to test")
         ; ("-min-depth", Arg.Set_int min_depth, "minimum trie depth")
         ; ("-art-ifreq", Arg.String handle_ifreq, "articulation frequency")
         ; ("-verbose", Arg.Set verbose, "print loop info")
         ])
    (fun anon_arg -> failwith ("Unexpected anonymous argument: " ^ anon_arg))
    usage
  in
  Set.run ()

let tune_gc () = ()

    (* Gc.set { (Gc.get ()) with Gc.major_heap_increment = 10
                ; Gc.max_overhead         = 10000000
                ; Gc.space_overhead       = 100
                ; Gc.allocation_policy    = 1
                }*)

let _ = tune_gc () ; main ()

