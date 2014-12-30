(** Adapton Keyed lists. *)

open AdaptonInternal
open Primitives

exception Not_yet_implemented

(** Functor to make Adapton lists, given a particular module for Adapton thunks. *)
module Make (M : Signatures.AType)
  : Signatures.AKListType
  with type atype = M.atype
  and  type 'a thunk = 'a M.thunk
  and  type 'a alist = [ `Cons of 'a * Key.t * 'b | `Nil ] M.thunk as 'b
  =
struct
  module ABool = M.Make(Types.Bool)

  (** Adapton lists containing ['a]. *)
  type 'a alist = 'a alist' M.thunk

  (** Constructor tags for Adapton lists containing ['a]. *)
  and 'a alist' = [ `Cons of 'a * Key.t * 'a alist | `Nil ]

  (** Types and operations common to Adapton lists containing any type. *)
  module T = struct
    (** Abstract type identifying the given module for Adapton thunks
        used to create this module for Adapton lists. *)
    type atype = M.atype

    (** Adapton thunks from the given module used to create this module for Adapton lists. *)
    type 'a thunk = 'a M.thunk

    (** True if this module implements Adapton lists. *)
    let is_incremental = M.is_incremental

    (** True if this module implements lazy lists. *)
    let is_lazy = M.is_lazy

    (** Return the id of an Adapton list. *)
    let id = M.id

    (** Compute the hash value of an Adapton list. *)
    let hash = M.hash

    (** Debugging string *)
    let string = M.string

    let sanitize = M.sanitize

    (** Compute whether two Adapton lists are equal. *)
    let equal = M.equal

    (** Return the tag of an Adapton list, (re-)computing it if necessary. *)
    let force = M.force

    (** Recompute Adapton lists if necessary. *)
    let refresh = M.refresh

    let flush = M.flush

    let viznode = M.viznode

    (** Create a regular list from an Adapton list. *)
    let to_list xs =
      let rec to_list acc xs = match force xs with
        | `Cons ( x, _, xs ) -> to_list (x::acc) xs
        | `Nil -> List.rev acc
      in
      to_list [] xs

    (** Create a regular list of ids of elements from an Adapton list. *)
    let to_ids xs =
      let rec to_ids acc xs = match force xs with
        | `Cons ( _, _, xs ) -> to_ids (id xs::acc) xs
        | `Nil -> List.rev (id xs::acc)
      in
      to_ids [] xs

    (** Create a regular list from the first [k] elements of an Adapton list. *)
    let take xs k =
      let rec take acc xs k = if k = 0 then List.rev acc else match force xs with
        | `Cons ( x, _, xs ) -> take (x::acc) xs (pred k)
        | `Nil -> List.rev acc
      in
      take [] xs k

    (** Return the head of an Adapton list. *)
    let hd xs = match force xs with
      | `Cons ( x, _, _ ) -> x
      | `Nil -> failwith "hd"

    (** Return the tail of an Adapton list. *)
    let tl xs = match force xs with
      | `Cons ( _, _, xs ) -> xs
      | `Nil -> failwith "tl"
  end
  include T

  (** Output module types of {!AKList.MakeBasic}. *)
  module type BasicS = Signatures.AKListType.BasicS

  (** Output module types of {!AKList.Make}. *)
  module type S = Signatures.AKListType.S

  (** Helper functor to make basic list constructors and combinators
      for Adapton lists of a specific type. *)
  module MakeBasic (R : ResultType)
    : BasicS
    with type atype    = atype
    and  type 'a thunk = 'a thunk
    and  type data = R.t
    and  type t    = R.t alist
    and  type t'   = R.t alist'
    =
  struct
    module L = M.Make (
      struct
        type t = R.t alist'

        let hash seed = function
          | `Cons (x, k, xs) -> hash (Key.hash (R.hash seed x) k) xs
          | `Nil -> Hashtbl.seeded_hash seed `Nil

        let equal xs xs' = xs == xs' || match xs, xs' with
          | `Cons ( h, k, t ), `Cons ( h', k', t' ) ->
              R.equal h h' && Key.equal k k' && equal t t'
          | _ -> false

        let string = function
          | `Nil            -> "Nil"
          | `Cons (h, k, t) -> Printf.sprintf "Cons(%s,%s,%s)"
              (R.string h) (Key.string k) (M.string t)

        let sanitize = function
          | `Nil            -> `Nil
          | `Cons (h, k, t) -> `Cons(R.sanitize h, k, M.sanitize t)
      end
    )

    (** Adapton thunks for a specific type, return by certain list operations. *)
    module AData = M.Make (R)

    (** Value contained by Adapton lists for a specific type. *)
    type data = R.t

    (** Adapton lists for a specific type. *)
    type t = L.t

    (** Tags for Adapton lists for a specific type. *)
    type t' = L.data

    include T

    (** Create an Adapton list from a constant list constructor that
        does not depend on other Adapton thunks. *)
    let const = L.const

    (** Update an Adapton list with a constant list constructor that
        does not depend on other Adapton thunks. *)
    let update_const = L.update_const

    (** Create an Adapton list from a thunk returning a list
        constructor that may depend on other Adapton thunks. *)
    let thunk = L.thunk

    (** Update an Adapton list with a thunk returning a list
        constructor that may depend on other Adapton thunks. *)
    let update_thunk = L.update_thunk

    let set  = L.set
    let cell = L.cell
    let viznode = L.viznode

    include MemoN.Make (
      struct
        type data = L.data
        type t = L.t

        (** Create memoizing constructor of an Adapton list. *)
        let memo = L.memo
      end)

    include MemoNK.Make (
      struct
        type data = L.data
        type t = L.t

        (** Create memoizing constructor of an Adapton list. *)
        let memo_keyed = L.memo_keyed
      end)

    (** Create an Adapton list from a regular list. *)
    let of_list ?(nondet=Key.nondet) xs =
      let rec of_list acc = function
        | [] -> acc
        | x::xs -> of_list (const (`Cons ( x, nondet (), acc ))) xs
      in
      of_list (const `Nil) (List.rev xs)

    (** Update the [i]th element of an Adapton list to insert a value [x]. *)
    let insert' i x k xs xs_tl =
      if i < 0 then invalid_arg "insert" ;
      let rec insert i xs = match force xs with
        | `Cons( _, _, xs ) when i > 0 -> insert (i-1) xs
        | `Nil when i > 0 -> failwith "insert"
        | xs_content ->
            update_const xs_tl xs_content ;
            update_const xs (`Cons(x,k,xs_tl))
      in
      insert i xs

    let insert i x k xs = insert' i x k xs (const `Nil)

    (** Update the [i]th element of an Adapton list to remove a value and return it. *)
    let remove' i xs =
      if i < 0 then invalid_arg "remove" ;
      let rec remove i xs = match force xs with
        | `Cons (_,_,xs) when i > 0 -> remove (i-1) xs
        | `Cons (x,k,ys) ->
            update_const xs (force ys) ;
            (x,k,ys)
        | `Nil -> failwith "remove"
      in
      remove i xs

    (** Update the [i]th element of an Adapton list to remove a value and return it. *)
    let remove i xs = let (x,k,_) = remove' i xs in (x,k)

    (** Update the head of an Adapton list to push a value in front. *)
    let push x k xs = insert 0 x k xs

    (** Update the head of an Adapton list to pop a value from the front. *)
    let pop xs = remove 0 xs

    (** Create memoizing constructor to map an Adapton list with a mapping function. *)
    let memo_map (type a) (type b)
        (module L : Signatures.AKListType.BasicS
           with type atype = atype and type data = a and type t = b) f =
      memo_keyed (module L) ~symbol:"map" begin
        fun map xs -> match L.force xs with
          | `Cons (x, k, xs) ->
              let k1,k2 = Key.fork k in
              `Cons (f x, k1, map k2 xs)
          | `Nil -> `Nil
      end

    (** Create memoizing constructor to map an Adapton list with a mapping function. *)
    let memo_map_with_key (type a) (type b)
        (module L : Signatures.AKListType.BasicS
           with type atype = atype and type data = a and type t = b)
        ( map_f : L.data -> Key.t -> data ) =
      memo_keyed (module L) ~symbol:"map_with_key" begin
        fun map xs -> match L.force xs with
          | `Nil -> `Nil
          | `Cons (x, k, xs) ->
              let k1,k  = Key.fork k in
              let k2,k3 = Key.fork k in
              let y = map_f x k1 (* Diff: provide key k1 to func f *) in
              `Cons (y, k2, map k3 xs)
      end

    let memo_filter (type a) (type b) ( filter_f : data -> bool ) =
      memo_keyed (module L) ~symbol:"filter" begin
        fun filter xs -> match L.force xs with
          | `Nil -> `Nil
          | `Cons (x, k, xs) ->
              let k1,k  = Key.fork k in
              let k2,k3 = Key.fork k in
              if filter_f x then
                `Cons (x, k2, filter k3 xs)
              else
                force (filter k3 xs)
      end

    let memo_contract contract_f ?bias:(b=2) =
      memo_keyed (module L) ~symbol:"contract" begin
        fun contract xs ->
          match force xs with
          | `Nil -> `Nil
          | `Cons (x, xk, ys) ->
              let (xk1,xk2) = Key.fork xk in
              if Key.flip ~bias:b xk then
                match L.force ys with
                  | `Nil -> `Cons(x, xk1, ys)
                  | `Cons (y, yk, zs) ->
                      `Cons(contract_f xk1 x y, xk2, contract yk zs)
              else
                `Cons(x, xk1, contract xk2 ys)
      end

    let memo_len_lte =
      (* Tests if len(list) <= len.  Returns a thunked boolean. *)
      (* Note: this memo table is globally shared. (this sharing is desirable.) *)
      ABool.memo_keyed2 (module L) (module Types.Int) ~symbol:"len_lte" begin
        fun len_lte xs len ->
          match force xs, len with
            | `Nil, _ -> true
            | `Cons(_, _,  _), 0 -> false
            | `Cons(_, xk, ys), n ->
                assert (n > 0);
                ABool.force (len_lte xk ys (n-1))
      end

    let memo_reduce contract_f ?bias:(b=2) =
      (* contract is hoisted so as to fix its memo table in this body's closure *)
      let contract = memo_contract contract_f ~bias:b in
      AData.memo_keyed2 (module Key) (module L) ~symbol:"reduce" begin
        fun reduce xs_key xs ->
          match force xs with
            | `Nil -> failwith "memo_reduce: empty list"
            | `Cons (x, _, xstl) ->
                begin match force xstl with
                  | `Nil -> x
                  | `Cons _ -> 
                      let (k1,k)  = Key.fork xs_key in
                      let (k2,k3) = Key.fork k in
                      let xs'     = contract k1 xs in
                      let cont    = reduce k2 k3 xs' in
                      AData.force ( cont )
                end
      end

    (** Create memoizing constructor to concatenate two Adapton lists. *)
    let memo_append _ = raise Not_yet_implemented

    (** Create memoizing constructor to filter an Adapton list with a predicate. *)
    (*     let memo_filter f = raise Not_yet_implemented  *)

    (** Create memoizing constructor to filter an Adapton list with a predicate and key. *)
    let memo_filter_with_key (type a)
        (module K : DatType with type t = a) f =
      raise Not_yet_implemented

    (** Create memoizing constructor to simultaneously filter and map
        an Adapton list with a predicate/mapping function. *)
    let memo_filter_map (type a) (type b)
        (module L : Signatures.AKListType.BasicS
           with type atype = atype and type data = a and type t = b) f =
      raise Not_yet_implemented

    (** Create memoizing constructor to scan (fold over prefixes of)
        an Adapton list with an scanning function. *)
    let memo_scan (type a) (type b)
        (module L : Signatures.AKListType.BasicS
           with type atype = atype and type data = a and type t = b) f =
      raise Not_yet_implemented

    (** Create memoizing constructor to tree-fold an Adapton list with
        an associative fold function. *)
    let memo_tfold f =
      raise Not_yet_implemented

    let tfold _ =
      raise Not_yet_implemented

  end


  (** Functor to make various list constructors and combinators for
      Adapton lists of a specific type. *)
  module Make (R : ResultType) : S
    with type atype = atype
    and type 'a thunk = 'a thunk
    and type data = R.t
    and type t = R.t alist
    and type t' = R.t alist' =
  struct
    module L = MakeBasic (R)
    include L


    (**/**) (* internal type of mergesort *)
    module RunType = MakeBasic (L)
      (**/**)

    (** Create memoizing constructor to quicksort an Adapton list with
        a comparator. *)
    let memo_quicksort cmp xs =
      raise Not_yet_implemented

    let memo_merge cmp =
      memo_keyed2 (module L) (module L) ~symbol:"merge" begin
        fun merge xs ys -> match (L.force xs), (L.force ys) with
          | `Nil, ys -> ys
          | xs, `Nil -> xs
          | `Cons (x,xk,xstl), `Cons (y,yk,ystl) ->
              incr Statistics.Counts.unit_cost ;
              if cmp x y <= 0 then (* x is lesser *)
                let (xk1,xk2) = Key.fork xk in
                `Cons (x, xk1, merge xk2 xstl ys)
              else
                let (yk1,yk2) = Key.fork yk in
                `Cons (y, yk1, merge yk2 xs ystl)
      end

    let memo_singletons =
      let nil = const `Nil in
      let single = memo_keyed2 (module R) (module Key) ~symbol:"single"
        (fun _ x xk -> `Cons (x, xk, nil))
      in
      RunType.memo_map_with_key (module L)
        (fun x k ->
           let k1,k2 = Key.fork k in
           single k1 x k2)

    (** Create memoizing constructor to mergesort an Adapton list with a comparator. *)
    let mergesort cmp ?bias:(b=2) xs_key xs =
      let reduce =
        RunType.memo_reduce (memo_merge cmp) ~bias:b
      in
      L.thunk begin fun () ->
        if ABool.force (L.memo_len_lte xs_key xs 1) then
          L.force xs
        else
          let (k1,k ) = Key.fork xs_key in
          let (k2,k3) = Key.fork k in
          let xs = memo_singletons k1 xs in
          let ys = reduce k2 k3 xs in
          L.force (RunType.AData.force ys)
      end

    let memo_mergesort_w_extra_test cmp ?bias:(b=2) =
      let reduce =
        RunType.memo_reduce (memo_merge cmp) ~bias:b
      in
      let ms = L.memo_keyed2 (module Key) (module L) ~symbol:"mergesort" begin fun _ k xs ->
        if ABool.force (L.memo_len_lte k xs 1) then
          L.force xs
        else
          let (k1,k ) = Key.fork k in
          let (k2,k3) = Key.fork k in
          let xs = memo_singletons k1 xs in
          let ys = reduce k2 k3 xs in
          L.force (RunType.AData.force ys)
      end
      in
      fun k xs ->
        let (k1,k2) = Key.fork k in
        ms k1 k2 xs

    let memo_mergesort cmp ?bias:(b=2) =
      let reduce =
        RunType.memo_reduce (memo_merge cmp) ~bias:b
      in
      let ms = 
        L.memo_keyed2 (module Key) (module L) 
          ~symbol:"mergesort" begin fun _ k xs ->
            match force xs with
              | `Nil -> `Nil
              | `Cons _ -> 
                  let (k1,k ) = Key.fork k in
                  let (k2,k3) = Key.fork k in
                  let xs = memo_singletons k1 xs in
                  let ys = reduce k2 k3 xs in
                  L.force (RunType.AData.force ys)
          end
      in
      fun k xs ->
        let (k1,k2) = Key.fork k in
        ms k1 k2 xs

  end
end
