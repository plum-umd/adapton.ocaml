(* http://en.wikipedia.org/wiki/Find_first_set *)
let ffs0 =
    (* zero-index, i.e., like ctz (count trailing zero) but returns 0 for 0 *)
    let shift = 8 in
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
    fun x -> if x = 0 then 0 else ffs x 0 - 1
