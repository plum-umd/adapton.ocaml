(** Mutable priority set based on simple binary tree. *)

module type S = sig
    type data
    type t
    exception Empty
    val create : unit -> t
    val top : t -> data option
    val add : t -> data -> bool
    val pop : t -> data
    val remove : t -> data -> bool
end

module Make (O : Set.OrderedType) = struct
    type data = O.t

    type t = t' ref
    and t' = Null | Node of data * t * t

    exception Empty

    let create () = ref Null

    let rec top queue = match !queue with
      | Node ( value, ({ contents=Node _ } as left), _ ) ->
         top left
      | Node ( value, { contents=Null }, right ) ->
         (Some value)
      | Null -> None

    let rec add queue x = match !queue with
        | Node ( value, left, right ) ->
            let dir = O.compare x value in
            if dir == 0 then
                false
            else if dir < 0 then
                add left x
            else
                add right x
        | Null ->
            queue := Node ( x, ref Null, ref Null );
            true

    let rec pop queue = match !queue with
        | Node ( value, ({ contents=Node _ } as left), _ ) ->
            pop left
        | Node ( value, { contents=Null }, right ) ->
            queue := !right;
            value
        | Null ->
            raise Empty

    let rec remove queue x = match !queue with
        | Node ( value, left, right ) ->
            let dir = O.compare x value in
            if dir == 0 then begin
                queue := (try Node ( pop right, left, right ) with Empty -> !left);
                true
            end else if dir < 0 then
                remove left x
            else
                remove right x
        | Null ->
            false
end
