(** Dynamic weak array. *)

type 'a t = {
    mutable size : int;
    mutable array : 'a Weak.t;
}

let create n = { size=0; array=Weak.create (max 1 n) }

let clear xs =
    let threshold = Weak.length xs.array * 2 / 3 in
    if xs.size < threshold then
        Obj.truncate (Obj.repr xs.array) (1 + threshold); (* from weak.ml *)
    xs.size <- 0

let add xs x =
    if Weak.length xs.array = xs.size then begin
        let array = Weak.create (xs.size * 3 / 2 + 1) in
        let j = ref 0 in
        for i = 0 to xs.size - 1 do
            match Weak.get xs.array i with
                | Some _ as x'opt -> Weak.set array !j x'opt; incr j
                | None -> ()
        done;
        xs.size <- !j;
        xs.array <- array
    end;
    Weak.set xs.array xs.size (Some x);
    xs.size <- xs.size + 1

let fold fn xs acc =
    let acc = ref acc in
    let j = ref 0 in
    for i = 0 to xs.size - 1 do
        match Weak.get xs.array i with
            | Some x as x'opt ->
                acc := fn x !acc;
                if !j < i then Weak.set xs.array !j x'opt;
                incr j
            | None ->
                ()
    done;
    xs.size <- !j;
    !acc
