(* loop-based *)
let rec ffs_loop x = if x = 0 then 0 else
    let rec loop t r =
        if (x land t) <> 0 then r
        else loop (t lsl 1) (r + 1)
    in loop 1 1

(* another loop-based *)
let rec ffs_loop2 x = if x = 0 then 0 else
    let rec loop r =
        if (x land (1 lsl r)) <> 0 then r + 1
        else loop (r + 1)
    in loop 0

(* char-table based *)
let ffs_table shift =
    let size = 1 lsl shift in
    let mask = size - 1 in
    let table = Bytes.init size begin fun x -> Char.unsafe_chr begin
        if x = 0 then 0 else
        let rec loop r = if (x lsr r) land 1 <> 0 then r else loop (r + 1) in loop 0 + 1
    end end in
    let rec ffs x r =
        let r' = Char.code (Bytes.unsafe_get table (x land mask)) in
        if r' <> 0 then r + r' else ffs (x lsr shift) (r + shift)
    in
    fun x -> if x = 0 then 0 else ffs x 0

(* same as ffs_table, to see the effect of code movement *)
let ffs_table_dup shift =
    let size = 1 lsl shift in
    let mask = size - 1 in
    let table = Bytes.init size begin fun x -> Char.unsafe_chr begin
        if x = 0 then 0 else
        let rec loop r = if (x lsr r) land 1 <> 0 then r else loop (r + 1) in loop 0 + 1
    end end in
    let rec ffs x r =
        let r' = Char.code (Bytes.unsafe_get table (x land mask)) in
        if r' <> 0 then r + r' else ffs (x lsr shift) (r + shift)
    in
    fun x -> if x = 0 then 0 else ffs x 0

(* another char-table based *)
let ffs_table2 shift =
    let size = 1 lsl shift in
    let mask = size - 1 in
    let table = Bytes.init size begin fun x -> Char.unsafe_chr begin
        if x = 0 then 0 else
        let rec loop r = if (x lsr r) land 1 <> 0 then r else loop (r + 1) in loop 0 + 1
    end end in
    let rec ffs x r =
        if r > 63 then 0 else
        let r' = Char.code (Bytes.unsafe_get table (x land mask)) in
        if r' <> 0 then r + r' else ffs (x lsr shift) (r + shift)
    in
    fun x -> ffs x 0

(* int-table based *)
let ffs_table_int shift =
    let size = 1 lsl shift in
    let mask = size - 1 in
    let table = Array.init size begin fun x ->
        if x = 0 then 0 else
        let rec loop r = if (x lsr r) land 1 <> 0 then r else loop (r + 1) in loop 0 + 1
    end in
    let rec ffs x r =
        let r' = table.(x land mask) in
        if r' <> 0 then r + r' else ffs (x lsr shift) (r + shift)
    in
    fun x -> if x = 0 then 0 else ffs x 0

(* switch-table based *)
let ffs_switch =
    let shift = 4 in
    let size = 1 lsl shift in
    let mask = size - 1 in
    let rec ffs x r =
        let r' = match x land mask with
            | 0 -> 0
            | 1 -> 1
            | 2 -> 2
            | 3 -> 1
            | 4 -> 3
            | 5 -> 1
            | 6 -> 2
            | 7 -> 1
            | 8 -> 4
            | 9 -> 1
            | 10 -> 2
            | 11 -> 1
            | 12 -> 3
            | 13 -> 1
            | 14 -> 2
            | 15 -> 1
            | _ -> assert false
        in
        if r' <> 0 then r + r' else ffs (x lsr shift) (r + shift)
    in
    fun x -> if x = 0 then 0 else ffs x 0

(* c-stub based *)
external ffs_ext : int -> int = "ml_ffs"
external ffs_ext_noalloc : int -> int = "ml_ffs" "noalloc"
external ffs_ext_intrinsic : int -> int = "ml_ffs_intrinsic"
external ffs_ext_intrinsic_noalloc : int -> int = "ml_ffs_intrinsic" "noalloc"
external ffs_ext_print : int -> int = "ml_ffs_print"

(* ocaml 4.03 *)
(* external ffs_ext_untagged : (int [@untagged]) -> (int [@untagged]) = "ffsl" *)
(* external ffs_ext_untagged_noalloc : (int [@untagged]) -> (int [@untagged]) = "ffsl" "noalloc" *)

open Benchmark

let () =
    (* sanity check *)
    for k = 0 to 63 do Format.printf "%016x %2d@." (1 lsl k) (ffs_ext_print (1 lsl k)) done;

    let time = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1 in
    let repeat = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 3 in
    Array.iter begin fun size ->
        let xs = Array.init size (fun _ -> (Random.bits () lsl 60) lxor (Random.bits () lsl 30) lxor Random.bits ()) in
        xs.(0) <- 0;
        xs.(1) <- max_int;
        xs.(2) <- min_int;
        let mapffs ffs = Array.map ffs xs in
        let runffs ffs = ignore @@ mapffs ffs in
        let factors = [
            ("ffs_loop", runffs, ffs_loop);
            ("ffs_loop2", runffs, ffs_loop2);
            ("ffs_table 8", runffs, ffs_table 8);
            ("ffs_table_dup 8", runffs, ffs_table_dup 8);
            ("ffs_table2 8", runffs, ffs_table2 8);
            ("Ffs.ffs_table 8", runffs, Ffs.ffs_table 8);
            ("ffs_table_int 8", runffs, ffs_table_int 8);
            ("ffs_ext", runffs, ffs_ext);
            ("ffs_ext_noalloc", runffs, ffs_ext_noalloc);
            ("ffs_ext_intrinsic", runffs, ffs_ext_intrinsic);
            ("ffs_ext_intrinsic_noalloc", runffs, ffs_ext_intrinsic_noalloc);
            (*("ffs_ext_untagged", runffs, ffs_ext_untagged);*)
            (*("ffs_ext_untagged_noalloc", runffs, ffs_ext_untagged_noalloc);*)
            ("ffs_switch", runffs, ffs_switch);
            ("ffs_table 7", runffs, ffs_table 7);
            ("ffs_table 6", runffs, ffs_table 6);
            ("ffs_table 5", runffs, ffs_table 5);
            ("ffs_table 4", runffs, ffs_table 4);
        ] in

        (* another sanity check *)
        List.iter begin fun ( label, _, ffs ) ->
            Format.printf "%s: %B@." label (mapffs ffs = mapffs ffs_ext)
        end factors;

        tabulate @@ throughputN ~repeat time factors
    end [| 100; 10000 |]
