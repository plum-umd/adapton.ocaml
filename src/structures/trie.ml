(* Adapted from Pugh POPL '89 *)

module type DataS = Data.S

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

end


module type S = sig
  
  type elt

  type name
         
  module rec D : Data.S
         and A : Art.S with type data = D.t
                        and type name = name

  include Data.S with type t = D.t

  val top_name : t -> name
  val is_empty : t -> bool
  val force : t -> t
  val cardinal : t -> int

  val nempty : ?min_depth:int -> t
  val nsingleton : ?min_depth:int -> name -> elt -> t
  val nadd : name -> t -> elt -> t
  val nunion : name -> t -> t -> t    

  val empty : ?min_depth:int -> t
  val singleton : ?min_depth:int -> elt -> t
  val add : t -> elt -> t
  val union : t -> t -> t

  val nof_list : ?min_depth:int -> name -> elt list -> t
  val of_list : ?min_depth:int -> elt list -> t
  val to_list : t -> elt list

  val find : (elt -> bool) -> t -> int -> elt option
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val structural_fold :
      (module DataS with type t = 'a) ->
      ?empty:(BS.t -> 'a -> 'a) ->
       ?atom:(BS.t -> elt -> 'a -> 'a) ->
       ?node:(BS.t -> 'a -> 'a -> 'a) ->
      string -> t -> 'a -> 'a

end

module MakePlace
  (Name: Name.S)
  (AL : ArtLib.S)
  (E : sig include Data.S val place : t -> int end)
  : S with type elt = E.t
       and type name = Name.t =
struct
  
  type elt = E.t

  type name = Name.t
  let name_of_data (type d) (module D : Data.S with type t = d) (d : d) : Name.t =
    Name.gensym (string_of_int (D.hash (Hashtbl.hash "Trie#name_of_data") d))
               
  module S = struct
    include Set.Make(E)
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

  type 'art _t =  Node of  BS.t * 'art _t * 'art _t
               |  Atom of  BS.t *  S.t
               | Empty of  BS.t
               |  Root of  int * 'art _t
               |  Name of Name.t * 'art _t
               |   Art of  'art

(*  let rec pp__t poly_art fmt =
    function
    | Node (a0,a1,a2) ->
      (Format.fprintf fmt "@[<hov2>Trie.MakePlace.Node (@,";
       (((BS.pp fmt) a0;
         Format.fprintf fmt ",@ ";
         (pp__t (fun fmt  -> poly_art fmt) fmt) a1);
        Format.fprintf fmt ",@ ";
        (pp__t (fun fmt  -> poly_art fmt) fmt) a2);
       Format.fprintf fmt "@])")
    | Atom (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>Trie.MakePlace.Atom (@,";
       ((BS.pp fmt) a0; Format.fprintf fmt ",@ "; (S.pp fmt) a1);
       Format.fprintf fmt "@])")
    | Empty a0 ->
      (Format.fprintf fmt "(@[<hov2>Trie.MakePlace.Empty@ ";
       (BS.pp fmt) a0;
       Format.fprintf fmt "@])")
    | Root (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>Trie.MakePlace.Root (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (pp__t (fun fmt  -> poly_art fmt) fmt) a1);
       Format.fprintf fmt "@])")
    | Name (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>Trie.MakePlace.Name (@,";
       (Format.fprintf fmt "%s" (Name.string a0);
        Format.fprintf fmt ",@ ";
        (pp__t (fun fmt  -> poly_art fmt) fmt) a1);
       Format.fprintf fmt "@])")
    | Art a0 ->
      (Format.fprintf fmt "(@[<hov2>Trie.MakePlace.Art@ ";
       (poly_art fmt) a0;
       Format.fprintf fmt "@])")
    and show__t poly_art x = Format.asprintf "%a" (pp__t poly_art) x*)

  let rec compare__t poly_art lhs rhs =
    match (lhs, rhs) with
    | (Node (lhs0,lhs1,lhs2),Node (rhs0,rhs1,rhs2)) ->
      (match BS.compare lhs0 rhs0 with
       | (-1)|1 as x -> x
       | _ ->
         (match (compare__t poly_art) lhs1 rhs1 with
          | (-1)|1 as x -> x
          | _ ->
            (match (compare__t poly_art) lhs2 rhs2 with
             | (-1)|1 as x -> x
             | _ -> 0)))
    | (Atom (lhs0,lhs1),Atom (rhs0,rhs1)) ->
      (match BS.compare lhs0 rhs0 with
       | (-1)|1 as x -> x
       | _ ->
         (match S.compare lhs1 rhs1 with | (-1)|1 as x -> x | _ -> 0))
    | (Empty lhs0,Empty rhs0) ->
      (match BS.compare lhs0 rhs0 with | (-1)|1 as x -> x | _ -> 0)
    | (Root (lhs0,lhs1),Root (rhs0,rhs1)) ->
      (match (fun (a : int)  -> fun b  -> Pervasives.compare a b) lhs0
               rhs0
       with
       | (-1)|1 as x -> x
       | _ ->
         (match (compare__t poly_art) lhs1 rhs1 with
          | (-1)|1 as x -> x
          | _ -> 0))
    | (Name (lhs0,lhs1),Name (rhs0,rhs1)) ->
      (match Name.compare lhs0 rhs0 with
       | (-1)|1 as x -> x
       | _ ->
         (match (compare__t poly_art) lhs1 rhs1 with
          | (-1)|1 as x -> x
          | _ -> 0))
    | (Art lhs0,Art rhs0) ->
      (match poly_art lhs0 rhs0 with | (-1)|1 as x -> x | _ -> 0)
    | _ ->
      let to_int =
        function
        | Node (_,_,_) -> 0
        | Atom (_,_) -> 1
        | Empty _ -> 2
        | Root (_,_) -> 3
        | Name (_,_) -> 4
        | Art _ -> 5 in
      ((fun (a : int)  -> fun b  -> Pervasives.compare a b))
        (to_int lhs) (to_int rhs)

  module rec D : Data.S with type t = A.t _t = struct

    type t = A.t _t

    let rec compare = compare__t A.compare

    let equal = ""
    
    let rec equal t t' = match t, t' with
      | Node (bs, t, u), Node (bs', t', u') ->
        BS.equal bs bs' && equal t t' && equal u u'
      | Atom (bs, es), Atom (bs', es') ->
        (BS.equal bs bs') &&
          (S.for_all (fun e -> S.exists (E.equal e) es') es) &&
          (S.for_all (fun e -> S.exists (E.equal e) es) es')
      | Empty bs, Empty bs' -> BS.equal bs bs'
      | Root (md, t), Root (md', t') ->
        md = md' && equal t t'
      | Art a, Art a' -> A.equal a a'
      | Name (nm, t), Name (nm', t') ->
        Name.equal nm nm'  && equal t t'
      | _ -> false

    let string : t -> string =
      let rec loop = function
        | Node (bs, t, t') -> (loop t)^" "^(loop t')
        | Atom (bs, es)    ->
          let esstr = S.string es in
          if String.length esstr > 4
          then "\n  "^(BS.show bs)^" "^esstr
          else String.sub esstr 2 ((String.length esstr)-2)
        | Empty bs         -> ""
        | Root (md, t)     -> "{"^(loop t)^"\n}"
        | Name (nm, t)     -> (Name.show nm) ^ ":" ^ (loop t)
        |  Art   a         -> (*loop (Art.force a)*) A.show a
      in
      loop

    let show = string
    let pp fmt s = Format.fprintf fmt "%s" (string s)

    let hash : int -> t -> int =
      let rec hash (seed : int) : t -> int = function
        | Node (bs, t, t') -> BS.hash (hash (hash seed t') t) bs
        | Atom (bs, es)    -> BS.hash (S.hash seed es) bs
        | Empty bs         -> BS.hash seed bs
        | Root (md, t)     -> Hashtbl.seeded_hash (hash seed t) md
        | Name (nm, t)     -> Name.hash (hash seed t) nm
        |  Art  a          -> A.hash seed a
      in
      hash

    let rec sanitize = function
      | Node (bs, t, t') -> Node (bs, sanitize t, sanitize t')
      | (Empty _ as t)   -> t
      | Root (md, t)     -> Root (md, sanitize t)
      | Name (nm, t)     -> Name (nm, sanitize t)
      |  Art   a         -> Art (A.sanitize a)
      | Atom (bs, es)    ->
        let es' = S.fold (fun e -> S.add (E.sanitize e)) es S.empty in
        Atom (bs, es')

  end
  and A : Art.S with type data = D.t
                 and type name = Name.t = AL.MakeArt(Name)(D)

  include D

  let ((thunk, pthunk, pfthunk)
      : (Name.t -> t -> t) *
        (Name.t -> t -> t) *
        (int -> Name.t -> t -> t)) =
    let ident =
      A.mk_mfn
        (Name.gensym "Trie.MakePlace#thunk")
        (module D)
        (fun _ t -> t)
    in
    let pfreq = 4 in
    (fun nm t ->
      let art = ident.A.mfn_nart nm t in
      ignore (A.force art) ;
      Name (nm, Art art)),
    (fun nm t ->
      if Random.int pfreq = 0
      then
        (let art = ident.A.mfn_nart nm t in
         ignore (A.force art) ;
         Name (nm, Art art))
      else
        t),
    (fun ifreq nm t ->
      let ifreq = if ifreq <= 0 then 1 else ifreq in
      if Random.int ifreq = 0
      then
        (let art = ident.A.mfn_nart nm t in
         ignore (A.force art) ;
         Name (nm, Art art))
      else
        t)

  let rec is_empty = function
    | Root (_, t) -> is_empty t
    | Art   a     -> is_empty (A.force a)
    | Name (_, t) -> is_empty t
    | Empty _     -> true
    | _           -> false

  let rec force x = match x with
    | Root (md, t) -> Root (md, force t)
    | Node (bs, t, t') -> Node (bs, force t, force t')
    | Art   a      -> force (A.force a)
    | Name (_, t)  -> force t
    | Empty _      -> x
    | Atom _       -> x

  let nempty : ?min_depth:int -> t =
    (fun ?(min_depth=1) ->
      let md = min_depth mod 32 in
      let nm0 = Name.gensym ("Adapton.Trie#empty" ^ (string_of_int md)) in
      let nm1, nm2 = Name.fork nm0 in
      pfthunk 0 nm1 (Root (md, pfthunk 0 nm2 (Empty (0, 0)))))[@warning "-16"]

  let empty : ?min_depth:int -> t =
    (fun ?(min_depth=1) ->
      let md = min_depth mod 32 in
      Root (md, Empty (0, 0)))[@warning "-16"]

  let top_name : t -> Name.t = function
    | Name (nm, _) -> nm
    | _ -> failwith "No name for top_name. (Did you `force' recently?)"

  let find (pred : elt -> bool) (t : t) (i : int) : elt option =
    let rec loop h = function
      | Empty bs         -> None
      | Node (bs, t, t') -> loop (h lsr 1) (if h mod 2 = 0 then t else t')
      |  Art   a         -> loop h (A.force a)
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
    |  Art  ar        -> fold f a (A.force ar)
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
        (Name.gensym ("Trie.MakeInc#structural_fold#"^namespace))
        (module Types.Tuple2(Out)(D))
        (fun loop (o, t) -> match t with
           | Node (bs, t, t') -> node bs (loop.IO.mfn_data (o, t)) (loop.IO.mfn_data (o, t'))
           | Empty bs         -> empty bs o
           | Atom (bs, es)    -> S.fold (atom bs) es o
           | Root (_, t)     -> loop.IO.mfn_data (o, t)
           |  Art   a         -> loop.IO.mfn_data (o, A.force a)
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
        "Trie.MakeInc#cardinal" in
    (fun t -> fold t 0)
  
  let rec split_atomic : t -> t =
    let suffix ((l, v) : BS.t) (k : int) : bool = v land k = v in
    function
    | ((Empty _) as t) | ((Node _) as t) -> t
    | Atom (bs, es) ->
      let zerobs = BS.prepend 0 bs in
      let  onebs = BS.prepend 1 bs in
      if suffix onebs (E.place (S.choose es)) (* <-- this choice assumes no hash collisions. *)
      then Node (bs, Empty zerobs, Atom (onebs, es))
      else Node (bs, Atom (zerobs, es), Empty onebs)
    | Name (_, Art _) as t -> print_endline (show t) ; assert false
    | Name (_, _) as t -> print_endline (show t) ; assert false
    | Art _ as t -> print_endline (show t) ; assert false
    | _ as t -> print_endline (show t) ; assert false

  (* Pairs keys if the inputs are named. *)
  let union : t -> t -> t =
    let rec loop = function
      | t, t' when equal t t' -> t'
      | Name (nm, Art a), Name (nm', Art a') ->
        let nm'' = Name.pair nm nm' in
        thunk nm'' (loop (A.force a, A.force a'))
      | Name (nm, Art a), t ->
        let nm = Name.pair nm (Name.gensym (show t)) in
        thunk nm (loop (A.force a, t))
      | t, Name (nm, Art a) ->
        let nm = Name.pair nm (Name.gensym (show t)) in
        thunk nm (loop (t, A.force a))
      | Root (md, t), Root (md', t') when md = md' ->
        Root (md, loop (t, t'))
      | Atom (bs, es), Atom (bs', es')
          (* these choices assume no hash collisions *)
        when E.place (S.choose es) = E.place (S.choose es') ->
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

  let nunion : Name.t -> t -> t -> t =
    let rec loop nm = function
      | t, t' when equal t t' -> t'
      | Empty _, t | t, Empty _ -> t
      | Name (_, Art a), Name (_, Art a') ->
        let nm, nm' = Name.fork nm in
        pthunk nm (loop nm' (A.force a, A.force a'))
      | Name (_, Art a), t ->
        let nm, nm' = Name.fork nm in
        pthunk nm (loop nm' (A.force a, t))
      | t, Name (nm, Art a) ->
        let nm, nm' = Name.fork nm in
        pthunk nm (loop nm' (t, A.force a))
      | Root (md, t), Root (md', t') when md = md' ->
        Root (md, loop nm (t, t'))
      | Atom (bs, es), Atom (bs', es')
          (* these choices assume no hash collisions *)
        when E.place (S.choose es) = E.place (S.choose es') ->
        Atom (bs, es')
      | t, t' ->
        (match split_atomic t, split_atomic t' with
        | sat, Empty _ | Empty _, sat -> sat
        | Node (bs, zerot, onet), Node (bs', zerot', onet') ->
          Node (bs,
                loop nm (zerot, zerot'),
                loop nm (onet,  onet'))
        | _ -> assert false (* split atomic only returns Node or Empty *))
    in
    fun nm t t' ->
      Random.init (Name.hash 42 nm) ;
      let nm, nm' = Name.fork nm in
      match loop nm' (t, t') with
      | Name _ as out -> out
      | u             -> thunk nm u
      
  (*let join (join : elt -> elt -> elt) : t -> t -> t =
    let rec loop = function
      | t, t' when equal t t' -> t'
      | Name (nm, Art a), Name (nm', Art a') ->
        let nm'' = Name.pair nm nm' in
        thunk nm'' (loop (Art.force a, Art.force a'))
      | Name (nm, Art a), t ->
        let nm = Name.pair nm (Name.gensym (show t)) in
        thunk nm (loop (Art.force a, t))
      | t, Name (nm, Art a) ->
        let nm = Name.pair nm (Name.gensym (show t)) in
        thunk nm (loop (t, Art.force a))

      | Root (md, t), Root (md', t') when md = md' ->
        Root (md, loop (t, t'))
      | Atom (bs, es), Atom (bs', es')
          (* these choices assume no hash collisions *)
        when E.place (S.choose es) = E.place (S.choose es') ->
        let lub = join (S.choose es) (S.choose es') in
        Atom (bs, S.singleton lub)
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
    fun t t' -> loop (t, t')*)

  let ifreq bs =
    let sqrtfreq = max 1 (BS.length bs) in
    sqrtfreq * sqrtfreq

  let internal_nadd : Name.t -> elt -> BS.t -> int -> t -> t =
    let rec loop (nm, e, bs, h, t) = match t with
      | Empty _ -> Atom (bs, S.singleton e)
      | Node (bs, t0, t1) ->
        if h mod 2 = 0
        then
          let t0 = loop (nm, e, (BS.prepend 0 bs), (h lsr 1), t0) in
          Node (bs, t0, t1)
        else               
          let t1 = loop (nm, e, (BS.prepend 1 bs), (h lsr 1), t1) in
          Node (bs, t0, t1)
      | Atom (bs, es) when E.place (S.choose es) = E.place e -> (* <-- assumes no collisions *)
        Atom (bs, S.add e (S.filter (fun e' -> E.place e' <> E.place e) es))
      | Name (_, Art a) ->
        let nm, nm' = Name.fork nm in
        let t = loop (nm', e, bs, h, A.force a) in
        pfthunk (ifreq bs) nm t
      | (Atom _) as t  -> loop (nm, e, bs, h, split_atomic t)
      | t -> assert false
    in
    fun nm e bs h t -> loop (nm, e, bs, h, t)

  let nadd_deep : Name.t -> int -> t -> elt -> t =
    let rec loop (nm, min, e, bs, h, m, t) = match t with
      | Empty _ when m = min -> Atom (bs, S.singleton e)
      | t'      when m = min -> internal_nadd nm e bs h t'
      | Empty _ ->
        let nm,  nm'  = Name.fork nm in
        let nm', nm'' = Name.fork nm' in
        if h mod 2 = 0
        then
          let zerobs = BS.prepend 0 bs in
          let t0 = loop (nm'', min, e, zerobs, (h lsr 1), (m+1), (Empty zerobs)) in
          let t1 = Empty (BS.prepend 1 bs) in
          Node (bs, pfthunk (ifreq bs) nm t0, pfthunk (ifreq bs) nm' t1)
        else
          let onebs = BS.prepend 1 bs in
          let t0 = Empty (BS.prepend 0 bs) in
          let t1 = loop (nm'', min, e, onebs, (h lsr 1), (m+1), (Empty onebs)) in
          Node (bs, pfthunk (ifreq bs) nm t0, pfthunk (ifreq bs) nm' t1)
      | Node (bs, t0, t1) ->
        if h mod 2 = 0
        then
          let t0 = loop (nm, min, e, (BS.prepend 0 bs), (h lsr 1), (m+1), t0) in
          Node (bs, t0, t1)
        else
          let t1 = loop (nm, min, e, (BS.prepend 1 bs), (h lsr 1), (m+1), t1) in
          Node (bs, t0, t1)
      | Name (_, Art a) -> (* <-- handling in a single case maintains the invariant *)
        let nm, nm' = Name.fork nm in (* that Names always surround As  *)
        pfthunk (ifreq bs) nm (loop (nm', min, e, bs, h, m, A.force a))
      | _ -> assert false
    (*| Atom _ -> assert false (* <-- Can't happen unless the minimum depth is violated  *)
      | Root _ -> assert false (* <-- Always unwrap the root in `[n]add`. *)*)
    in
    fun nm md t e ->
      let h = E.place e in
      Random.init (Name.hash 42 nm) ; (* seed pthunk based on the just name? should the elem be included? *)
      loop (nm, md, e, (0, 0), h, 1, t)

  let rec nadd nm t e = match t with
    | Name (_, Art a) -> (match A.force a with
                          | Root (md, t) ->
                            let nm, nm' = Name.fork nm in
                            let t' = nadd_deep nm' md t e in
                            thunk nm (Root (md, t'))
                          | t -> failwith ("malformed t: " ^ (show t)))
    | Root (md, t) ->
      let nm, nm' = Name.fork nm in
      let t' = nadd_deep nm' md t e in
      Root (md, t')
    | _ -> assert false (* <-- user code can't hold on to just a Node, Atom, or Empty *)

  let internal_add : elt -> BS.t -> int -> t -> t =
    let rec loop (e, bs, h, t) = match t with
      | Empty _ -> Atom (bs, S.singleton e)
      | Node (bs, t0, t1) ->
        if h mod 2 = 0
        then
          let t0 = loop (e, (BS.prepend 0 bs), (h lsr 1), t0) in
          Node (bs, t0, t1)
        else               
          let t1 = loop (e, (BS.prepend 1 bs), (h lsr 1), t1) in
          Node (bs, t0, t1)
      | Atom (bs, es) when E.place (S.choose es) = E.place e -> (* <-- assumes no collisions *)
        Atom (bs, S.add e (S.filter (fun e' -> E.place e' <> E.place e) es))
      | (Atom _) as t  -> loop (e, bs, h, split_atomic t)
      | t -> assert false
    in
    fun e bs h t -> loop (e, bs, h, t)

  let add_deep : int -> t -> elt -> t =
    let rec loop (min, e, bs, h, m, t) = match t with
      | Empty _ when m = min -> Atom (bs, S.singleton e)
      | t'      when m = min -> internal_add e bs h t'
      | Empty _ ->
        if h mod 2 = 0
        then
          let zerobs = BS.prepend 0 bs in
          let t0 = loop (min, e, zerobs, (h lsr 1), (m+1), (Empty zerobs)) in
          let t1 = Empty (BS.prepend 1 bs) in
          Node (bs, t0, t1)
        else
          let onebs = BS.prepend 1 bs in
          let t0 = Empty (BS.prepend 0 bs) in
          let t1 = loop (min, e, onebs, (h lsr 1), (m+1), (Empty onebs)) in
          Node (bs, t0, t1)
      | Node (bs, t0, t1) ->
        if h mod 2 = 0
        then
          let t0 = loop (min, e, (BS.prepend 0 bs), (h lsr 1), (m+1), t0) in
          Node (bs, t0, t1)
        else
          let t1 = loop (min, e, (BS.prepend 1 bs), (h lsr 1), (m+1), t1) in
          Node (bs, t0, t1)
      | _ -> assert false
    in
    fun md t e -> loop (md, e, (0, 0), E.place e, 1, t)

  let rec add t e = match t with
    | Root (md, t) ->
      let t' = add_deep md t e in
      Root (md, t')
    | _ -> assert false

  let nsingleton ?(min_depth = 1) nm (e : elt) : t =
    nadd nm (nempty ~min_depth) e

  let singleton ?(min_depth = 1) (e : elt) : t =
    add (empty ~min_depth) e

  let of_list ?(min_depth = 1) : elt list -> t =
    List.fold_left add (empty ~min_depth)

  let nof_list ?(min_depth = 1) nm (l : elt list) : t =
    let out, _ =
      List.fold_left
        (fun (out, nm) e ->
           let nm, nm' = Name.fork nm in
           (nadd nm out e, nm'))
        (empty ~min_depth, nm)
        l
    in
    out

  let to_list : t -> elt list =
    let fold = 
      structural_fold
        (module Types.List(E))
        ~atom:(fun _ e a -> e::a)
        ~node:(fun _ a b -> a@b)
        "Trie.Make#to_list" in
    (fun t -> fold t [])

end

module Make(N : Name.S)(A : ArtLib.S)(E : Data.S) =
  MakePlace(N)(A)(struct include E let place t = hash _PLACEMENT_SEED t end)

module Set = struct

  module type S = sig
    include S
    val mem : t -> elt -> bool
    val subsumes : ?order:(elt -> elt -> bool) -> t -> t -> bool
  end

  module Make(Name : Name.S)(A : ArtLib.S)(E : Data.S)
    : S with type elt = E.t
         and type name = Name.t =
  struct

    include Make(Name)(A)(E)

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

  module type S = sig
    type k
    type v
    type name
    module rec D : Data.S
    and A : Art.S with type data = D.t
                   and type name = name
    include Data.S with type t = D.t

    val top_name : t -> name
    val is_empty : t -> bool
    val force : t -> t
    val cardinal : t -> int

    val nempty : ?min_depth:int -> t
    val nsingleton : ?min_depth:int -> name -> k -> v -> t
    val nadd : name -> t -> k -> v -> t
    val nunion : name -> t -> t -> t    

    val empty : ?min_depth:int -> t
    val singleton : ?min_depth:int -> k -> v -> t
    val add : t -> k -> v -> t
    val union : t -> t -> t

    val nof_list : ?min_depth:int -> name -> (k * v) list -> t
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

  module Make
    (N : Name.S)
    (A : ArtLib.S)
    (K : Data.S)
    (V : Data.S)
    : S with type k = K.t
         and type v = V.t
         and type name = N.t = struct 

    let place (k, _) = K.hash _PLACEMENT_SEED k

    include MakePlace
    (N)
    (A)
    (struct
      include Types.Tuple2(K)(V)
      let show (k, v) = "["^(K.show k)^" -> "^(V.show v)^"]"
      let pp fmt s = Format.fprintf fmt "%s" (show s)
      let equal (k, v) (k', v') = K.equal k k' && V.equal v v'
      let hash seed (k, v) = K.hash (V.hash seed v) k
      let place = place
      let compare t t' = Pervasives.compare (hash 42 t) (hash 42 t')
    end)
        
    type k = K.t
    type v = V.t

    let singleton ?(min_depth = 1) (k : k) (v : v) : t = singleton ~min_depth (k, v)
    let nsingleton ?(min_depth = 1) nm (k : k) (v : v) : t = nsingleton ~min_depth nm (k, v)

    let add (t : t) (k : k) (v : v) : t = add t (k, v)
    let nadd (n : name) (t : t) (k : k) (v : v) : t = nadd n t (k, v)

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

  module type S = sig
    type sv
    module Vs : Set.S with type elt = sv
    include Map.S with type v = Vs.t
    val top_name : t -> name
    val    njoin : name -> t -> k -> sv -> t
    val     join : t -> k -> sv -> t
    val nclobber : name -> t -> k -> sv -> t
    val  clobber : t -> k -> sv -> t
    val   branch : ('b -> 'a -> 'b) -> 'b -> t -> k -> (sv -> 'a) -> 'b
    val   svfold : ('a -> k -> sv -> 'a) -> 'a -> t -> 'a
  end

  module Make
    (N : Name.S)
    (A : ArtLib.S)
    (K : Data.S)
    (V : Data.S)
    : S with type k = K.t
         and type sv = V.t
         and type name = N.t = struct
    
    type sv = V.t
    module Vs = Set.Make(N)(A)(V)
    module M = Map.Make(N)(A)(K)(Vs)
    include M
    let name_of_data (type d) (module D : DataS with type t = d) (d : d) : name =
      N.gensym (string_of_int (D.hash (Hashtbl.hash "Trie#name_of_data") d))

    let nclobber : name -> t -> k -> sv -> t =
      fun nm t k v ->
        let nm, nm' = N.fork nm in
        nadd nm t k (Vs.nsingleton nm' v)

    let clobber : t -> k -> sv -> t =
      fun t k v ->
        add t k (Vs.singleton v)

    let njoin : name -> t -> k -> sv -> t =
      fun nm t k v ->
        let nm, nm' = N.fork nm in
        let vs' = match find t k with
          | Some vs -> Vs.nadd nm vs v
          | None    -> Vs.nsingleton nm v
        in
        nadd nm' t k vs'

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

end


module Graph = struct

  module type S = sig

    type vertex
    include Rel.S with type  k = vertex
                   and type sv = vertex

    val top_name : t -> name
    val mem_vertex : t -> vertex -> bool
    val mem_edge   : t -> vertex -> vertex -> bool
    val nadd_edge : name -> t -> vertex -> vertex -> t
    val to_dot : ?attrs:(vertex -> (string * string) list) -> t -> string
    val fold_edges  : ('a -> vertex -> vertex -> 'a) -> 'a -> t -> 'a
    val fold_vertex : ('a -> vertex -> 'a) -> 'a -> t -> 'a
    val nb_edges  : t -> int
    val nb_vertex : t -> int

  end

  module Make
    (N : Name.S)
    (A : ArtLib.S)
    (V : Data.S)
    : S with type vertex = V.t
         and type name = N.t = struct
    
    type vertex = V.t
    include Rel.Make(N)(A)(V)(V)

    let mem_edge t v v' = match find t v with
      | Some vs -> Vs.fold (fun a v'' -> a || V.equal v' v'') false vs
      | None    -> false

    let mem_vertex = mem
    let nadd_edge nm t v v' =
      if mem_vertex t v'
      then njoin nm t v v'
      else
        let nm, nm' = N.fork nm in
        njoin nm' (nadd nm t v' (Vs.nempty ~min_depth:1)) v v'

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
            

    (*let show t =
      string_of_list
        (fun (v, vs) -> Printf.sprintf "(%s %s)" (V.string v) (Vs.string vs))
        (to_list t)
    let pp ff p = Format.pp_print_string ff (show p)
      let string = show*)

  end

end
