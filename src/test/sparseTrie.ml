let min_depth : int ref              = ref 1
let art_ifreq : Trie.Meta.Freq.t ref = ref (`Const 1.)
let size      : int ref              = ref 10
let data_type : string ref           = ref "ocaml-set"
let verbose   : bool ref             = ref false

module Set =
struct
  let nm = Name.gensym

  module type TSET =
  sig
    type t
    type elt
    val mt : unit -> t
    val add : t -> elt -> t
    val mem : t -> elt -> bool
    val cardinal : t -> int
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val to_list : t -> elt list
    val name : string
    val show : t -> string
  end

  module NTrie =
  struct
    module S = Trie.Set.Make(Name)(Insts.Nominal)(Types.Int)
    type t = S.t
    type elt = int
    let cardinal = S.cardinal
    let mt () = S.empty ~art_ifreq:!art_ifreq ~min_depth:!min_depth
    let add t e = S.add (nm()) t e
    let mem = S.mem
    let fold = S.fold
    let to_list = S.to_list
    let name = "ntrie-set"
    let show = S.show
  end
  module FSTrie =
  struct
    module S = Trie.Set.MakeNonInc(Name)(Insts.FromScratch)(Types.Int)
    type t = S.t
    type elt = int
    let cardinal = S.cardinal
    let mt () = S.empty ~min_depth:!min_depth
    let mem = S.mem
    let add = S.add
    let fold = S.fold
    let to_list = S.to_list
    let name = "trie-set"
    let show = S.show
  end
  module OCaml =
  struct
    module S = Set.Make(Types.Int)
    type t = S.t
    type elt = int
    let mt () = S.empty
    let cardinal = S.cardinal
    let add t e = S.add e t
    let mem t e = S.mem e t
    let fold f a t = S.fold (fun n a -> f a n) t a
    let to_list t = S.fold (fun n a -> n::a) t []
    let name = "ocaml-set"
    let show = fun _ -> "ocaml-set!"
  end

  module Test(S : TSET with type elt = int) =
  struct

    let fold_up f a n =
      let rec loop a = function
        | m when m < n -> loop (f a m) (m+1)
        | _ -> a
      in
      loop a 0

    let fill =
      let check = !size / 10 in
      fold_up
        (fun acc n ->
           (if n mod check = 0 then
              Printf.printf "filled %i.\n%!" n) ;
           S.add acc n)
        (S.mt ())

    module OS = Set.Make(Types.Tuple2(Types.Int)(Types.String))

    let test () =
      (if !verbose then
         Printf.printf "Filling %s (md=%i, af=%s) set with %i elements.\n%!"
           S.name !min_depth (Trie.Meta.Freq.show !art_ifreq) !size) ;

      let s = fill !size in
      let c = S.fold (fun a _ -> a+1) 0 s in

      (if !verbose then
                   Printf.printf "%s : found %i / %i elements\n%!"
                   (if c = !size then "SUCCESS" else "FAILURE")
                   c !size) ;
      (if c <> !size then
         let c =(fold_up
            (fun c n ->
              (if not (S.mem s n)
               then Printf.printf "Missing %i\n%!" (n+1)) ;
                c+1)
            0
            !size)
         in Printf.printf "Checked %i\n%!" c)
      (* ignore (S.fold (fun _ n -> Printf.printf "found: %i\n%!" n) () s) *)
      (* Printf.printf "Converting to list to check size.\n%!" ;
      let l = S.to_list s in
      if List.length l = !size
      then print_endline " success!"
      else print_endline " failure!"*)

  end

  let run () =
    let ocamlset = (module OCaml : TSET with type elt = int) in
    let insts =
      [    NTrie.name, (module NTrie    : TSET with type elt = int)
      ;   FSTrie.name, (module FSTrie   : TSET with type elt = int)
      ]
    in
    let module S =
      (val try List.assoc !data_type insts with Not_found -> ocamlset)
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
          let i = String.index s ',' in
          `Depth (int_of_string (String.sub s 1 (i-1)),
                  int_of_string (String.sub s (i+1) (len-i-1)))
        else if len > 0 && s.[0] = 'f' then
          `First (int_of_string (String.sub s 1 (len-1)))
        else `Const (float_of_string s)
      with _ ->
        failwith ("Bad art_ifreq, expected positive int or \"depth\", but got " ^ s)
    in
    Printf.printf "HANDLEIFREQ: %s => %s" s (Trie.Meta.Freq.show ifreq) ;
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

