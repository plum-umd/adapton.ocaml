(* Adapted from Pugh POPL '89 *)

module type DataS = Data.S
module type ArtS = Art.S

(* Used to deterministically place elements in a Trie. *)
let _PLACEMENT_SEED = 42

(* this module structure needs work, Grifola is already
   two modules deep. *)
(*module MakeArt = Grifola.Default.ArtLib.MakeArt(Name)*)

(* Bit strings. Type is length * value so bit strings with leading
 * zeros aren't conflated.
 *)
module BS : sig
  include Data.S with type t = int * int
  val pow     : int -> int -> int
  val flip    : int -> int -> int
  val is_set  : int -> int -> bool
  val prepend : int -> t -> t
  val length  : t -> int
  val max_len : int
end = struct
  type t = int * int
  [@@deriving eq, ord]
  let show (((l, v) as bs) : t) : string =
    if l = 0 then "''"
    else
      let rec loop a = function
        | 0, v -> a
        | l, v -> loop ((if v mod 2 = 0 then "0" else "1")^a) (l-1, v lsr 1) in
      loop "" bs
  let pp fmt s = Format.fprintf fmt "%s" (show s)
  let hash = Hashtbl.seeded_hash
  let sanitize x = x

  let length = fst
  
  let rec pow (b : int) : int -> int = function
    | 0 -> 1 | 1 -> b
    | n -> let x = pow b (n / 2) in
           x*x*(if n mod 2 = 0 then 1 else b)

  let flip (i : int) (b : int) : int =
    let n = pow 2 i in 
    (if n land b = n then (-) else (+)) b (pow 2 i)

  let is_set (i : int) (b : int) : bool =
    let n = flip i 0 in n land b = n

  let prepend (b : int) ((l, v) : t) : t = match b with
    | 0 when not (is_set l v) -> (l+1, v)
    | 1 when      is_set l v  -> (l+1, v)
    | 0 | 1 -> (l+1, flip l v)
    | _ -> failwith "b has to be a bit (0 or 1)"

  let max_len = 30
end

(* metadata held by the root node *)
module Meta =
struct
  module Freq =
  struct
    type t = [ `Never
             | `Depth of int * int
             | `First of int
             | `Const of int
             ]
    [@@deriving eq, ord]
    let show = function
      | `Never   -> "0"
      | `Depth (i, j) -> Printf.sprintf "(depth^%i)/%i" i j
      | `First n -> "first " ^ (string_of_int n)
      | `Const i -> string_of_int i
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let hash s = function
      | `Never   -> Hashtbl.seeded_hash s
                      "Adapton.Trie.Meta.Freq#`Never"
      | `Depth (i, j) -> Hashtbl.seeded_hash
                           (Hashtbl.seeded_hash
                              (Hashtbl.seeded_hash s
                                 "Adapton.Trie.Meta.Freq#`Depth")
                              i)
                           j
      | `First i -> Hashtbl.seeded_hash
                      (Hashtbl.seeded_hash s
                         "Adapton.Trie.Meta.Freq#`First")
                      i
      | `Const i -> Hashtbl.seeded_hash
                      (Hashtbl.seeded_hash s
                         "Adapton.Trie.Meta.Freq#`Const")
                      i
    let ifreq bs = function
      | `Never        -> 0
      | `Depth (i, j) -> (BS.pow (BS.length bs) i) / j
      | `First  i     -> if i > (BS.length bs) then 1 else 0
      | `Const  i     -> i
  end
  type t =
    { min_depth : int
    ; art_ifreq  : Freq.t
    } [@@deriving eq, ord, show]
  let hash s { min_depth ; art_ifreq } =
    Freq.hash
      (Hashtbl.seeded_hash
         (Hashtbl.seeded_hash s "Adapton.Trie.Meta")
         min_depth)
      art_ifreq
  let sanitize { min_depth ; art_ifreq } = { min_depth ; art_ifreq }
end


module type NonIncS =
sig
  
  type elt
  include Articulated.S
  val is_empty : t -> bool
  val force : t -> t
  val cardinal : t -> int

  val empty : ?min_depth:int -> t
  val singleton : ?min_depth:int -> elt -> t
  val add : t -> elt -> t
  val union : t -> t -> t

  val of_list : ?min_depth:int -> elt list -> t
  val to_list : t -> elt list

  val find : (elt -> bool) -> t -> int -> elt option
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val structural_fold :
    (module DataS with type t = 'a) ->
    ?empty:(BS.t -> 'a  -> 'a) ->
     ?atom:(BS.t -> elt -> 'a -> 'a) ->
     ?node:(BS.t -> 'a  -> 'a -> 'a) ->
    string -> t -> 'a -> 'a

end


module type IncS =
sig
  
  type elt
  include Articulated.S
  val top_name : t -> name
  val is_empty : t -> bool
  val force : t -> t
  val cardinal : t -> int

  val empty : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> name -> t
  val singleton : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> name -> elt -> t
  val add : name -> t -> elt -> t
  val union : name -> t -> t -> t    

  val of_list : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> name -> elt list -> t
  val to_list : t -> elt list

  val find : (elt -> bool) -> t -> int -> elt option
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val structural_fold :
    (module DataS with type t = 'a) ->
    ?empty:(BS.t -> 'a  -> 'a) ->
     ?atom:(BS.t -> elt -> 'a -> 'a) ->
     ?node:(BS.t -> 'a  -> 'a -> 'a) ->
    string -> t -> 'a -> 'a

end
  

module MakePlace
  (Name: Name.S)
  (AL : ArtLib.S)
  (E :
   sig
     include Data.S
     val place_hash  : t -> int
     val place_equal : t -> t -> bool
   end) =
struct
  
  type elt = E.t

  type name = Name.t
               
  module S = struct
    include Set.Make(struct
        type t = E.t
        let compare a b =
          if E.place_equal a b then 0
          else E.compare a b
      end)
    let string s =
      let sep = ", " in
      let n, elts = fold (fun x (n, a) -> (n+1, (E.show x)^sep^a)) s (0, "") in
      if n > 0 then
        "{ "^(String.sub elts 0
                ((String.length elts)-(String.length sep)))^" }"
      else "{}"             
    let hash seed s = fold (fun x a -> E.hash a x) s seed
    let pp fmt s = Format.fprintf fmt "%s" (string s)
  end

  type 'art _t =  Node of   BS.t * 'art _t * 'art _t
               |  Atom of   BS.t *  S.t
               | Empty of   BS.t
               |  Root of Meta.t * 'art _t
               |  Name of Name.t * 'art _t
               |   Art of  'art
  [@@deriving eq, ord, show]

  module rec D : Data.S with type t = Art.t _t = struct

    type t = Art.t _t
    [@@deriving ord]

    let show : t -> string =
      let rec loop path = function
        | Node (bs, t, t') -> (loop (path ^ "0") t)^" "^(loop (path ^ "1") t')
        | Atom (bs, es)    ->
          let esstr = S.string es in
          if String.length esstr > 4
          then "\n  "^path^" "^esstr
          else String.sub esstr 2 ((String.length esstr)-2)
        | Empty bs         -> ""
        | Root (md, t)     -> "{"^(loop path t)^"\n}"
        | Name (nm, t)     -> loop path t
        |  Art   a         -> loop (path ^ "@") (Art.force a)
      in
      loop ""

    let pp fmt s = Format.fprintf fmt "%s" (show s)
    
    let rec equal t t' = match t, t' with
      | Node (bs, t, u), Node (bs', t', u') ->
        BS.equal bs bs' && equal t t' && equal u u'
      | Atom (bs, es), Atom (bs', es') ->
        (BS.equal bs bs') &&
          (S.for_all (fun e -> S.exists (E.equal e) es') es) &&
          (S.for_all (fun e -> S.exists (E.equal e) es) es')
      | Empty bs, Empty bs' -> BS.equal bs bs'
      | Root (md, t), Root (md', t') ->
        Meta.equal md md' && equal t t'
      | Art a, Art a' -> Art.equal a a'
      | Name (nm, t), Name (nm', t') ->
        Name.equal nm nm'  && equal t t'
      | _ -> false

    let hash : int -> t -> int =
      let rec hash (seed : int) : t -> int = function
        | Node (bs, t, t') -> BS.hash (hash (hash seed t') t) bs
        | Atom (bs, es)    -> BS.hash (S.hash seed es) bs
        | Empty bs         -> BS.hash seed bs
        | Root (md, t)     -> Hashtbl.seeded_hash (hash seed t) md
        | Name (nm, t)     -> Name.hash (hash seed t) nm
        |  Art  a          -> Art.hash seed a
      in
      hash

    let rec sanitize = function
      | Node (bs, t, t') -> Node (bs, sanitize t, sanitize t')
      | (Empty _ as t)   -> t
      | Root (md, t)     -> Root (md, sanitize t)
      | Name (nm, t)     -> Name (nm, sanitize t)
      |  Art   a         -> Art (Art.sanitize a)
      | Atom (bs, es)    ->
        let es' = S.fold (fun e -> S.add (E.sanitize e)) es S.empty in
        Atom (bs, es')

  end
  and Art : ArtS with type data = D.t
                  and type name = Name.t = AL.MakeArt(Name)(D)

  include D

  let rec force x = match x with
    | Root (md, t) -> Root (md, force t)
    | Node (bs, t, t') -> Node (bs, force t, force t')
    | Art   a      -> force (Art.force a)
    | Name (_, t)  -> force t
    | Empty _      -> x
    | Atom _       -> x

  let top_name : t -> Name.t = function
    | Name (nm, _) -> nm
    | _ -> failwith "No name for top_name. (Did you `force' recently?)"

  let find (pred : elt -> bool) (t : t) (i : int) : elt option =
    let rec loop h = function
      | Empty bs         -> None
      | Node (bs, t, t') -> loop (h lsr 1) (if h mod 2 = 0 then t else t')
      |  Art   a         -> loop h (Art.force a)
      | Atom (bs, es)    ->
        S.fold (fun n -> function None when pred n -> Some n | a -> a) es None
      | Name (_,  t) | Root (_, t) -> loop h t
    in
    loop i t

  let rec fold (f : 'a -> elt -> 'a) (a : 'a) : t -> 'a = function
    | Node (_, t, t') -> fold f (fold f a t) t'
    | Empty _         -> a
    | Atom (_, es)    -> S.fold (fun n a -> f a n) es a
    | Root (_, t)     -> fold f a t
    |  Art  ar        -> fold f a (Art.force ar)
    | Name (_, t)     -> fold f a t

  let structural_fold
      (type o)
      (module Out : Data.S with type t = o)
      ?(empty = ((fun bs a   -> a) : BS.t -> o   -> o))
      ?(atom  = ((fun bs e a -> a) : BS.t -> elt -> o -> o))
      ?(node  = ((fun bs a b -> a) : BS.t -> o -> o -> o))
      (namespace : string)
    : t -> o -> o =
    let module IO = AL.MakeArt(Name)(Out) in
    let loop = IO.mk_mfn
        (Name.of_string ("Trie.MakePlace#structural_fold#"^namespace))
        (module Types.Tuple2(Out)(D))
        (fun loop (o, t) -> match t with
           | Node (bs, t, t') -> node bs (loop.IO.mfn_data (o, t)) (loop.IO.mfn_data (o, t'))
           | Empty bs         -> empty bs o
           | Atom (bs, es)    -> S.fold (atom bs) es o
           | Root (_, t)     -> loop.IO.mfn_data (o, t)
           |  Art   a         -> loop.IO.mfn_data (o, Art.force a)
           | Name (nm, t)     ->
             IO.force (loop.IO.mfn_nart nm (o, t)))
    in
    (fun t o -> loop.IO.mfn_data (o, t))

  let cardinal : t -> int =
    let fold =
      structural_fold
        (module Types.Int)
        ~atom:(fun _ e a -> a+1)
        ~node:(fun _ a b -> a+b)
        "Trie.MakePlace#cardinal" in
    (fun t -> fold t 0)
  
  let rec split_atomic : t -> t =
    let suffix ((l, v) : BS.t) (k : int) : bool = v land k = v in
    function
    | ((Empty _) as t) | ((Node _) as t) -> t
    | Atom (bs, es) ->
      let zerobs = BS.prepend 0 bs in
      let  onebs = BS.prepend 1 bs in
      if suffix onebs (E.place_hash (S.choose es))
      then Node (bs, Empty zerobs, Atom (onebs, es))
      else Node (bs, Atom (zerobs, es), Empty onebs)
    | Name (_, Art _) as t -> print_endline (show t) ; assert false
    | Name (_, _) as t -> print_endline (show t) ; assert false
    | Art _ as t -> print_endline (show t) ; assert false
    | _ as t -> print_endline (show t) ; assert false
  
  let rec is_empty = function
    | Root (_, t) -> is_empty t
    | Art   a     -> is_empty (Art.force a)
    | Name (_, t) -> is_empty t
    | Empty _     -> true
    | _           -> false

  let to_list : t -> elt list =
    let fold = 
      structural_fold
        (module Types.List(E))
        ~atom:(fun _ e a -> e::a)
        ~node:(fun _ a b -> a@b)
        "Trie.MakePlace#to_list" in
    (fun t -> fold t [])

end
  

module MakeNonIncPlace
  (Name: Name.S)
  (AL : ArtLib.S)
  (E :
   sig
     include Data.S
     val place_hash  : t -> int
     val place_equal : t -> t -> bool
   end) =
struct

  include MakePlace(Name)(AL)(E)

  let empty : ?min_depth:int -> t =
    (fun ?(min_depth=1) ->
       (assert (min_depth > 0)) ;
       (if min_depth > 31 then
          (Printf.printf "Cannot make Adapton.Trie with min_depth > 31 (given %i)."
             min_depth)) ;
       let min_depth = min min_depth 31 in
       let meta = { Meta.min_depth ; Meta.art_ifreq=`Never } in
       Root (meta, Empty (0, 0)))[@warning "-16"]

  let union : t -> t -> t =
    let rec loop = function
      | t, t' when equal t t' -> t'
      | Root (md, t), Root (md', t') when not (Meta.equal md md') ->
        Printf.printf "Metadata:\n%s\n%s\n%!"
          (Meta.show md) (Meta.show md') ;
        failwith "Cannot union tries with different metadata."
      | Root (md, t), Root (md', t') when Meta.equal md md' ->
        Root (md, loop (t, t'))
      | Atom (bs, es), Atom (bs', es') when S.equal es es' ->
        Atom (bs, es')
      | Empty _, t | t, Empty _ -> t
      | t, t' ->
        (match split_atomic t, split_atomic t' with
        | sat, Empty _ | Empty _, sat -> sat
        | Node (bs, zerot, onet), Node (bs', zerot', onet') ->
          Node (bs,
                loop (zerot, zerot'),
                loop (onet,  onet'))
        | _ -> assert false (* split atomic only returns Node or Empty *))
    in
    fun t t' -> loop (t, t')


  let add : t -> elt -> t =
    let rec loop ({ Meta.min_depth=md } as m) bs h t e = match t with
      | Node (bs, t0, t1) ->
        let h' = h lsr 1 in
        if h mod 2 = 0
        then Node (bs, loop m (BS.prepend 0 bs) h' t0 e, t1)
        else Node (bs, t0, loop m (BS.prepend 1 bs) h' t1 e)
      | Empty _ when BS.length bs < md ->
        let  h' = h lsr 1 in
        let bs0 = BS.prepend 0 bs in
        let bs1 = BS.prepend 1 bs in
        let mt0 = Empty bs0 in
        let mt1 = Empty bs1 in
        if h mod 2 = 0
        then Node (bs, loop m bs0 h' mt0 e, mt1)
        else Node (bs, mt0, loop m bs1 h' mt1 e)
      | Empty _ -> Atom (bs, S.singleton e)
      | Atom (_, es) ->
        let depth = BS.length bs in
        if depth >= BS.max_len || S.exists (E.place_equal e) es
        then Atom (bs, S.add e (S.filter (fun e' -> not (E.place_equal e e')) es))
        else if depth < BS.max_len
        then loop m bs h (split_atomic t) e
        else (Printf.printf "Bad value found in add:\n%s\n" (show t) ; assert false)
      | t -> Printf.printf "Bad value found in add:\n%s\n" (show t) ; assert false
    in
    fun t e -> match t with
      | Root (m, t) -> Root (m, loop m (0, 0) (E.place_hash e) t e)
      | _ -> Printf.printf "Bad value found in add:\n%s\n" (show t) ; assert false

  let singleton ?(min_depth = 1) (e : elt) : t =
    add (empty ~min_depth) e

  let of_list ?(min_depth = 1) : elt list -> t =
    List.fold_left add (empty ~min_depth)

end
  

module MakeIncPlace
  (Name: Name.S)
  (AL : ArtLib.S)
  (E :
   sig
     include Data.S
     val place_hash  : t -> int
     val place_equal : t -> t -> bool
   end) =
struct

  include MakePlace(Name)(AL)(E)

  let thunk : Name.t -> t -> t =
    let ident =
      Art.mk_mfn
        (Name.of_string "Trie.MakeIncPlace#thunk")
        (module D)
        (fun _ t -> t)
    in
    fun nm t ->
      let art = ident.Art.mfn_nart nm t in
      ignore (Art.force art) ;
      Name (nm, Art art)

  let empty : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> Name.t -> t =
    (fun ?(art_ifreq=`Const 1) ?(min_depth=1) nm ->
       (assert (min_depth > 0)) ;
       (if min_depth > BS.max_len then
          (Printf.printf "Cannot make Adapton.Trie with min_depth > %i (given %i)."
             BS.max_len min_depth)) ;
       let min_depth = min min_depth 31 in
       let meta = { Meta.min_depth=min_depth ; Meta.art_ifreq=art_ifreq } in
       let nm0 =
         Name.of_string
           (Printf.sprintf "Adapton.Trie#empty:af=%s:md=%i"
              (Meta.Freq.show art_ifreq) min_depth)
       in
       let nm1, nm2 = Name.fork (Name.pair nm0 nm) in
       let mtbs = (0, 0) in
       thunk nm1 (Root (meta, thunk nm2 (Empty mtbs))))
      [@warning "-16"]

  let union : Name.t -> t -> t -> t =
    let mfn = Art.mk_mfn
        (Name.of_string "Trie.MakeIncPlace.nunion#mfn")
        (module Types.Tuple3(Name)(D)(D))
        (fun mfn (nm, t, t') -> match t, t' with
           | t, t' when equal t t' -> t'
           | Empty _, t | t, Empty _ -> t
           | Name (_, Art a), Name (_, Art a') ->
             let nm, nm' = Name.fork nm in
             let a = mfn.Art.mfn_nart nm (nm', Art.force a, Art.force a') in
             ignore (Art.force a) ;
             Name (nm, Art a)
           | Name (_, Art a), t ->
             let nm, nm' = Name.fork nm in
             let a = mfn.Art.mfn_nart nm (nm', Art.force a, t) in
             ignore (Art.force a) ;
             Name (nm, Art a)
           | t, Name (nm, Art a) ->
             let nm, nm' = Name.fork nm in
             let a = mfn.Art.mfn_nart nm (nm', t, Art.force a) in
             ignore (Art.force a) ;
             Name (nm, Art a)
           | Root (md, t), Root (md', t') when md = md' ->
             Root (md, mfn.Art.mfn_data (nm, t, t'))
           | Atom (bs, es), Atom (bs', es') when S.equal es es' ->
             Atom (bs, es')
           | t, t' ->
             (match split_atomic t, split_atomic t' with
              | sat, Empty _ | Empty _, sat -> sat
              | Node (bs, zerot, onet), Node (bs', zerot', onet') ->
                let nm, nm' = Name.fork nm in
                Node (bs,
                      mfn.Art.mfn_data (nm,  zerot, zerot'),
                      mfn.Art.mfn_data (nm', onet,  onet'))
              | _ -> assert false (* split atomic only returns Node or Empty *)))
    in
    fun nm t t' ->
      Random.init (Name.hash 42 nm) ;
      mfn.Art.mfn_data (nm, t, t')

  let add : Name.t -> t -> elt -> t =
    let module In =
      Types.Tuple6(Name)(Meta)(BS)(Types.Int)(D)(E)
    in
    let pmfn mfn (_, m,  bs,  _,  t,  _) ((nm', m', bs', h', t', e') as i') =
      let ifreq = Meta.Freq.ifreq bs' m.Meta.art_ifreq in
      if ifreq > 0 && Random.int ifreq = 0
      then let nm, nm' = Name.fork nm' in
           let a = mfn.Art.mfn_nart nm (nm', m', bs', h', t', e') in
           (ignore (Art.force a) ;
            Name (nm, Art a))
      else mfn.Art.mfn_data i'
    in
    let mfn = Art.mk_mfn
        (Name.of_string "Trie.MakeIncPlace.internal_add#mfn")
        (module In)
        (fun mfn ((nm, ({ Meta.min_depth=md ; Meta.art_ifreq=ai } as m), bs, h, t, e) as i) ->
           match t with
           | Name (_, Art a) -> mfn.Art.mfn_data (nm, m, bs, h, Art.force a, e)
           | Node (bs, t0, t1) ->
             let h' = h lsr 1 in
             if h mod 2 = 0
             then Node (bs, pmfn mfn i (nm, m, (BS.prepend 0 bs), h', t0, e), t1)
             else Node (bs, t0, pmfn mfn i (nm, m, (BS.prepend 1 bs), h', t1, e))
           | Empty _ when BS.length bs < md ->
             let  h' = h lsr 1 in
             let bs0 = BS.prepend 0 bs in
             let bs1 = BS.prepend 1 bs in
             let mt0 = Empty bs0 in
             let mt1 = Empty bs1 in
             if h mod 2 = 0
             then Node (bs, pmfn mfn i (nm, m, bs0, h', mt0, e), mt1)
             else Node (bs, mt0, pmfn mfn i (nm, m, bs1, h', mt1, e))
           | Empty _ -> Atom (bs, S.singleton e)
           | Atom (_, es) ->
             let depth = BS.length bs in
             if depth >= BS.max_len || S.exists (E.place_equal e) es
             then Atom (bs, S.add e (S.filter (fun e' -> not (E.place_equal e e')) es))
             else if depth < BS.max_len
             then mfn.Art.mfn_data (nm, m, bs, h, split_atomic t, e)
             else (Printf.printf "Bad value found in nadd:\n%s\n" (show t) ; assert false)
           | t -> Printf.printf "Bad value found in nadd:\n%s\n" (show t) ; assert false
        )
    in
    let root_mfn = Art.mk_mfn
        (Name.of_string "Trie.MakeIncPlace.nadd#root_mfn")
        (module Types.Tuple3(Name)(D)(E))
        (fun _ (nm, t, e) -> 
           match t with
           | Name (_, Art a) ->
             (match Art.force a with
              | Root (m, t') ->
                let nm,  nm'  = Name.fork nm in
                let i = (nm', m, (0, 0), (E.place_hash e), t', e) in
                let a = mfn.Art.mfn_nart nm i in
                ignore (Art.force a) ;
                Root (m, Name (nm, Art a))
         | t -> failwith (Printf.sprintf "Non-root node at entry to `Trie.MakeIncPlace.nadd':\n%s"
                            (show t)))
      | _ -> failwith (Printf.sprintf "Non-name node at entry to `Trie.MakeIncPlace.nadd':\n%s"
                         (show t)))
    in
    fun nm t e ->
      (* seed pthunk based on the just name? should the elem be included? *)
      Random.init (Name.hash 42 nm) ;
      let nm, nm' = Name.fork nm in
      let a = root_mfn.Art.mfn_nart nm (nm', t, e) in
      ignore (Art.force a) ;
      Name (nm, Art a)

  let singleton ?(art_ifreq=`Const 1) ?(min_depth = 1) nm (e : elt) : t =
    let nm, nm' = Name.fork nm in
    add nm' (empty ~art_ifreq ~min_depth nm) e

  let of_list ?(art_ifreq=`Const 1) ?(min_depth = 1) nm (l : elt list) : t =
    let nm, nm' = Name.fork nm in
    let out, _ =
      List.fold_left
        (fun (out, nm) e ->
           let nm, nm' = Name.fork nm in
           (add nm out e, nm'))
        (empty ~art_ifreq ~min_depth nm, nm')
        l
    in
    out

end

module MakeNonInc(N : Name.S)(A : ArtLib.S)(E : Data.S)
  : NonIncS with type name = N.t
             and type  elt = E.t =
  MakeNonIncPlace(N)(A)(struct
    include E
    let place_hash t = hash _PLACEMENT_SEED t
    let place_equal = equal
  end)

module MakeInc(N : Name.S)(A : ArtLib.S)(E : Data.S)
  : IncS with type name = N.t
          and type  elt = E.t =
  MakeIncPlace(N)(A)(struct
    include E
    let place_hash t = hash _PLACEMENT_SEED t
    let place_equal = equal
  end)

module Set =
struct

  module type NonIncS = sig
    include NonIncS
    val mem : t -> elt -> bool
    val subsumes : ?order:(elt -> elt -> bool) -> t -> t -> bool
  end

  module type IncS = sig
    include IncS
    val mem : t -> elt -> bool
    val subsumes : ?order:(elt -> elt -> bool) -> t -> t -> bool
  end

  module MakeNonInc(Name : Name.S)(A : ArtLib.S)(E : Data.S)
    : NonIncS with type elt = E.t
               and type name = Name.t =
  struct

    include MakeNonInc(Name)(A)(E)

    let mem (t : t) : elt -> bool =
      (fun e -> match find (E.equal e) t (E.hash _PLACEMENT_SEED e) with
         | Some _ -> true
         | None   -> false)

    let string t = "set "^(show t)

    let subsumes ?(order = (fun x x' -> E.compare x x' >= 0))
        (t : t) (t' : t) : bool =
      fold
        (fun a x' -> a || fold (fun a x -> a && order x x') a t)
        false
        t'
        
  end

  module MakeInc(Name : Name.S)(A : ArtLib.S)(E : Data.S)
    : IncS with type elt = E.t
            and type name = Name.t =
  struct

    include MakeInc(Name)(A)(E)

    let mem (t : t) : elt -> bool =
      (fun e -> match find (E.equal e) t (E.hash _PLACEMENT_SEED e) with
         | Some _ -> true
         | None   -> false)

    let string t = "set "^(show t)

    let subsumes ?(order = (fun x x' -> E.compare x x' >= 0))
        (t : t) (t' : t) : bool =
      fold
        (fun a x' -> a || fold (fun a x -> a && order x x') a t)
        false
        t'
        
  end

end

module Map = struct

  module type NonIncS =
  sig
    type k
    type v
    include Articulated.S

    val is_empty : t -> bool
    val force : t -> t
    val cardinal : t -> int

    val empty : ?min_depth:int -> t
    val singleton : ?min_depth:int -> k -> v -> t
    val add : t -> k -> v -> t
    val union : t -> t -> t

    val of_list : ?min_depth:int -> (k * v) list -> t
    val to_list : t -> (k * v) list

    val find : t -> k -> v option
    val mem : t -> k -> bool
    val fold : ('a -> k -> v -> 'a) -> 'a -> t -> 'a
    val structural_fold :
      (module DataS with type t = 'a) ->
      ?empty:(BS.t -> 'a  -> 'a) ->
       ?atom:(BS.t -> k * v -> 'a -> 'a) ->
       ?node:(BS.t -> 'a  -> 'a -> 'a) ->
      string -> t -> 'a -> 'a
  end

  module type IncS = sig
    type k
    type v
    include Articulated.S

    val top_name : t -> name
    val is_empty : t -> bool
    val force : t -> t
    val cardinal : t -> int

    val empty : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> name -> t
    val singleton : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> name -> k -> v -> t
    val add : name -> t -> k -> v -> t
    val union : name -> t -> t -> t    

    val of_list : ?art_ifreq:Meta.Freq.t -> ?min_depth:int -> name -> (k * v) list -> t
    val to_list : t -> (k * v) list

    val find : t -> k -> v option
    val mem : t -> k -> bool
    val fold : ('a -> k -> v -> 'a) -> 'a -> t -> 'a
    val structural_fold :
      (module DataS with type t = 'a) ->
      ?empty:(BS.t -> 'a  -> 'a) ->
       ?atom:(BS.t -> k * v -> 'a -> 'a) ->
       ?node:(BS.t -> 'a  -> 'a -> 'a) ->
      string -> t -> 'a -> 'a
  end

  module MakeNonInc
    (N : Name.S)
    (A : ArtLib.S)
    (K : Data.S)
    (V : Data.S)
    : NonIncS with type k = K.t
               and type v = V.t
               and type name = N.t = struct 

    let place (k, _) = K.hash _PLACEMENT_SEED k

    include MakeNonIncPlace
    (N)
    (A)
    (struct
      include Types.Tuple2(K)(V)
      let show (k, v) = "["^(K.show k)^" -> "^(V.show v)^"]"
      let pp fmt s = Format.fprintf fmt "%s" (show s)
      let equal (k, v) (k', v') = K.equal k k' && V.equal v v'
      let hash seed (k, v) = K.hash (V.hash seed v) k
      let place_hash = place
      let place_equal (k, _) (k', _) = K.equal k k'
      let compare t t' = Pervasives.compare (hash 42 t) (hash 42 t')
    end)
        
    type k = K.t
    type v = V.t

    let singleton ?(min_depth = 1) (k : k) (v : v) : t = singleton ~min_depth (k, v)

    let add (t : t) (k : k) (v : v) : t = add t (k, v)

    let fold f = fold (fun a (k, v) -> f a k v)

    let find (t : t) : k -> v option =
      (fun k -> match find (fun (k', _) -> K.equal k k') t (K.hash _PLACEMENT_SEED k) with
      | Some (_, v) -> Some v
      | None        -> None)

    let mem (t : t) : k -> bool =
      (fun k -> match find t k with
      | Some _ -> true
      | None   -> false)

  end

  module MakeInc
    (N : Name.S)
    (A : ArtLib.S)
    (K : Data.S)
    (V : Data.S)
    : IncS with type k = K.t
            and type v = V.t
            and type name = N.t = struct 

    let place (k, _) = K.hash _PLACEMENT_SEED k

    include MakeIncPlace
    (N)
    (A)
    (struct
      include Types.Tuple2(K)(V)
      let show (k, v) = "["^(K.show k)^" -> "^(V.show v)^"]"
      let pp fmt s = Format.fprintf fmt "%s" (show s)
      let equal (k, v) (k', v') = K.equal k k' && V.equal v v'
      let hash seed (k, v) = K.hash (V.hash seed v) k
      let place_hash = place
      let place_equal (k, _) (k', _) = K.equal k k'
      let compare t t' = Pervasives.compare (hash 42 t) (hash 42 t')
    end)
        
    type k = K.t
    type v = V.t

    let singleton ?(art_ifreq = `Const 1) ?(min_depth = 1) nm (k : k) (v : v) : t =
      singleton ~art_ifreq ~min_depth nm (k, v)

    let add (n : name) (t : t) (k : k) (v : v) : t = add n t (k, v)

    let fold f = fold (fun a (k, v) -> f a k v)

    let find (t : t) : k -> v option =
      (fun k -> match find (fun (k', _) -> K.equal k k') t (K.hash _PLACEMENT_SEED k) with
      | Some (_, v) -> Some v
      | None        -> None)

    let mem (t : t) : k -> bool =
      (fun k -> match find t k with
      | Some _ -> true
      | None   -> false)

  end

end

module Rel = struct

  module type NonIncS = sig
    type sv
    type name
    module Vs : Set.NonIncS with type  elt = sv
                             and type name = name
    include Map.NonIncS with type    v  = Vs.t
                         and type name := name
    val    join : t -> k -> sv -> t
    val clobber : t -> k -> sv -> t
    val  branch : ('b -> 'a -> 'b) -> 'b -> t -> k -> (sv -> 'a) -> 'b
    val  svfold : ('a -> k -> sv -> 'a) -> 'a -> t -> 'a
  end

  module type IncS = sig
    type sv
    type name
    module Vs : Set.IncS with type  elt = sv
                          and type name = name
    include Map.IncS with type    v  = Vs.t
                      and type name := name
    val top_name : t -> name
    val     join : name -> t -> k -> sv -> t
    val  clobber : name -> t -> k -> sv -> t
    val   branch : ('b -> 'a -> 'b) -> 'b -> t -> k -> (sv -> 'a) -> 'b
    val   svfold : ('a -> k -> sv -> 'a) -> 'a -> t -> 'a
  end

  module MakeNonInc
    (N : Name.S)
    (A : ArtLib.S)
    (K : Data.S)
    (V : Data.S)
    : NonIncS with type k = K.t
               and type sv = V.t
               and type name = N.t = struct
    
    type sv = V.t
    module Vs = Set.MakeNonInc(N)(A)(V)
    module M = Map.MakeNonInc(N)(A)(K)(Vs)
    include M

    let clobber : t -> k -> sv -> t =
      fun t k v ->
        add t k (Vs.singleton v)

    let join : t -> k -> sv -> t =
      fun t k v ->
        let vs' = match find t k with
          | Some vs -> Vs.add vs v
          | None    -> Vs.singleton v
        in
        add t k vs'

    let branch
        (type a)
        (type b)
        (add   : (b -> a -> b))
        (empty : b) : t -> k -> (sv -> a) -> b =
      fun t k f ->
        match find t k with
        | None -> empty
        | Some vs -> Vs.fold (fun a n -> add a (f n)) empty vs

    let svfold (type a) (f : a -> k -> sv -> a) : a -> t -> a =
      fold (fun a k -> Vs.fold (fun a v -> f a k v) a)

  end

  module MakeInc
    (N : Name.S)
    (A : ArtLib.S)
    (K : Data.S)
    (V : Data.S)
    : IncS with type k = K.t
            and type sv = V.t
            and type name = N.t = struct
    
    type sv = V.t
    module Vs = Set.MakeInc(N)(A)(V)
    module M = Map.MakeInc(N)(A)(K)(Vs)
    include M

    let clobber : name -> t -> k -> sv -> t =
      fun nm t k v ->
        let nm, nm' = N.fork nm in
        add nm t k (Vs.singleton nm' v)

    let join : name -> t -> k -> sv -> t =
      fun nm t k v ->
        let nm, nm' = N.fork nm in
        let vs' = match find t k with
          | Some vs -> Vs.add nm vs v
          | None    -> Vs.singleton nm v
        in
        add nm' t k vs'

    let branch
        (type a)
        (type b)
        (add   : (b -> a -> b))
        (empty : b) : t -> k -> (sv -> a) -> b =
      fun t k f ->
        match find t k with
        | None -> empty
        | Some vs -> Vs.fold (fun a n -> add a (f n)) empty vs

    let svfold (type a) (f : a -> k -> sv -> a) : a -> t -> a =
      fold (fun a k -> Vs.fold (fun a v -> f a k v) a)

  end

end


module Graph = struct

  module type NonIncS = sig

    type vertex
    include Rel.NonIncS with type  k = vertex
                         and type sv = vertex

    val mem_vertex : t -> vertex -> bool
    val mem_edge   : t -> vertex -> vertex -> bool
    val add_edge : t -> vertex -> vertex -> t
    val to_dot : ?attrs:(vertex -> (string * string) list) -> t -> string
    val fold_edges  : ('a -> vertex -> vertex -> 'a) -> 'a -> t -> 'a
    val fold_vertex : ('a -> vertex -> 'a) -> 'a -> t -> 'a
    val nb_edges  : t -> int
    val nb_vertex : t -> int

  end

  module type IncS = sig

    type vertex
    include Rel.IncS with type  k = vertex
                      and type sv = vertex

    val top_name : t -> name
    val mem_vertex : t -> vertex -> bool
    val mem_edge   : t -> vertex -> vertex -> bool
    val add_edge : name -> t -> vertex -> vertex -> t
    val to_dot : ?attrs:(vertex -> (string * string) list) -> t -> string
    val fold_edges  : ('a -> vertex -> vertex -> 'a) -> 'a -> t -> 'a
    val fold_vertex : ('a -> vertex -> 'a) -> 'a -> t -> 'a
    val nb_edges  : t -> int
    val nb_vertex : t -> int

  end

  module MakeNonInc
    (N : Name.S)
    (A : ArtLib.S)
    (V : Data.S)
    : NonIncS with type vertex = V.t
               and type name = N.t = struct
    
    type vertex = V.t
    include Rel.MakeNonInc(N)(A)(V)(V)

    let mem_edge t v v' = match find t v with
      | Some vs -> Vs.fold (fun a v'' -> a || V.equal v' v'') false vs
      | None    -> false

    let mem_vertex = mem
    let add_edge t v v' =
      if mem_vertex t v'
      then join t v v'
      else join (add t v' (Vs.empty ~min_depth:1)) v v'

    let fold_edges  (type a) f a t : a = svfold f a t
    let fold_vertex (type a) f a : t -> a = fold (fun a k _ -> f a k) a

    let nb_vertex = cardinal
    let nb_edges = fold_edges (fun n _ _ -> n+1) 0

    let string_of_list ?(sep=" ") ?(border=(fun s -> "(" ^ s ^ ")")) soe l =
      if List.length l > 0 then
        let elts = List.fold_right (fun elt a -> (soe elt)^sep^a) l "" in
        border (String.sub elts 0 ((String.length elts)-(String.length sep)))
      else border ""

    let to_dot
        ?(attrs=(fun _ -> []))
        g =
      let lline id v =
        let styles =
          string_of_list ~sep:", " ~border:(fun s -> "["^s^"]")
            (fun (k, v) -> k^"="^v)
            (attrs v)
        in
        Printf.sprintf "  %i %s;\n" id styles in
      let tbl : (vertex, int) Hashtbl.t = Hashtbl.create 100 in
      let labels, _ =
        fold_vertex
          (fun (labels, id) v ->
            Hashtbl.add tbl v id ;
            (labels ^ (lline id v), id+1))
          ("", 0)
          g
      in
      let eline v v' =
        Printf.sprintf "  %i -> %i;\n"
          (Hashtbl.find tbl v)
          (Hashtbl.find tbl v')
      in
      let edges =
        fold_edges
          (fun edges v v' -> edges ^ (eline v v'))
          ""
          g
      in
      Printf.sprintf "digraph {\n%s\n%s}\n" labels edges

  end

  module MakeInc
    (N : Name.S)
    (A : ArtLib.S)
    (V : Data.S)
    : IncS with type vertex = V.t
            and type name = N.t = struct
    
    type vertex = V.t
    include Rel.MakeInc(N)(A)(V)(V)

    let mem_edge t v v' = match find t v with
      | Some vs -> Vs.fold (fun a v'' -> a || V.equal v' v'') false vs
      | None    -> false

    let mem_vertex = mem
    let add_edge nm t v v' =
      if mem_vertex t v'
      then join nm t v v'
      else
        let nm, nm'   = N.fork nm  in
        let nm', nm'' = N.fork nm' in
        join nm'' (add nm' t v' (Vs.empty ~min_depth:1 nm)) v v'

    let fold_edges  (type a) f a t : a = svfold f a t
    let fold_vertex (type a) f a : t -> a = fold (fun a k _ -> f a k) a

    let nb_vertex = cardinal
    let nb_edges = fold_edges (fun n _ _ -> n+1) 0

    let string_of_list ?(sep=" ") ?(border=(fun s -> "(" ^ s ^ ")")) soe l =
      if List.length l > 0 then
        let elts = List.fold_right (fun elt a -> (soe elt)^sep^a) l "" in
        border (String.sub elts 0 ((String.length elts)-(String.length sep)))
      else border ""

    let to_dot
        ?(attrs=(fun _ -> []))
        g =
      let lline id v =
        let styles =
          string_of_list ~sep:", " ~border:(fun s -> "["^s^"]")
            (fun (k, v) -> k^"="^v)
            (attrs v)
        in
        Printf.sprintf "  %i %s;\n" id styles in
      let tbl : (vertex, int) Hashtbl.t = Hashtbl.create 100 in
      let labels, _ =
        fold_vertex
          (fun (labels, id) v ->
            Hashtbl.add tbl v id ;
            (labels ^ (lline id v), id+1))
          ("", 0)
          g
      in
      let eline v v' =
        Printf.sprintf "  %i -> %i;\n"
          (Hashtbl.find tbl v)
          (Hashtbl.find tbl v')
      in
      let edges =
        fold_edges
          (fun edges v v' -> edges ^ (eline v v'))
          ""
          g
      in
      Printf.sprintf "digraph {\n%s\n%s}\n" labels edges

  end

end
