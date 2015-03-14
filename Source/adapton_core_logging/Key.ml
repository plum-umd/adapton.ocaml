(* http://en.wikipedia.org/wiki/Find_first_set *)
(* TODO: Find a faster way to do this that's still portable;
   e.g., on x86 there's a special machine instruction. *)
let rec ffs x =
  if x = 0 then 0
  else
    let rec loop t r =
      if (x land t) = 0 then r
      else loop (t lsl 1) (r + 1)
    in loop 1 0

module type KEY = sig
  type t
  val gensym : string -> t (* User takes affinity into their own hands when choosing strings.. *)
  val pair   : t -> t -> t (* For creating conjunctive names *)
  val make_nondet : int -> (unit -> t)
  val nondet : unit -> t
  val fork   : t -> t * t
  val forkn  : int -> t -> t list
  val hash   : int -> t -> int
  val compare : t -> t -> int
  val equal  : t -> t -> bool
  val flip   : ?bias:int -> t -> bool
  val string : t -> string
  val sanitize : t -> t
  val height : t -> int
end

type label = int
type seed = int
  
type tree =
| Symbol of string
| Pair of tree * tree
| Label of label * seed (* seed randomizes the hash/flip values, to make it easier to generate and reproduce short test cases *)
| ForkL of tree
| ForkR of tree
    
type t = { hash:int;
           height:int;
           tree:tree}
  
let gensym s = 
  let h = Hashtbl.hash (Symbol s) in
  { hash = h ; 
    height = ffs h ; 
    tree=Symbol s }
    
let pair k1 k2 =     
  let min x y = if x < y then x else y in    
  { hash = Hashtbl.seeded_hash k1.hash k2.hash ;
    height = min k1.height k2.height ;
    tree = Pair(k1.tree,k2.tree) }
    
let rec string_of_tree = function 
  | Symbol(s) -> s
  | Pair(k1,k2) -> "("^(string_of_tree k1)^","^(string_of_tree k2)^")"
  | Label(l,s) -> "k"^(string_of_int l)^":"^(string_of_int s)      
  | ForkL(k) -> (string_of_tree k)^"L"
  | ForkR(k) -> (string_of_tree k)^"R"
    
let string k = string_of_tree k.tree
  
  (* XXX -- Monotonically-increasing values; not determined by seed *)
  (* Opens issue of how to have a seed-determined symgen function; but
     it fixes a correctness issue in the correctness test suite: When we
     reuse a seed used before, and reuse junk from the global memo tables
     erroneously *)
  (* TODO -- address this issue more directly; perhaps give a way to clear memo tables? *)
let next_label = ref 0
  
let make_nondet seed =
  fun () ->
    let l = !next_label in
    incr next_label ;
    assert ( !next_label > l ) ; (* Overflow is an error. *)
    let hash = Hashtbl.hash (Label (l, seed)) in
    { hash = hash ;
      height = ffs hash ;
      tree = Label ( l, seed ) }
      
let nondet = make_nondet 0
  
let fork k =
  let k1_hash = Hashtbl.seeded_hash 1111 k.hash in
  let k2_hash = Hashtbl.seeded_hash 2222 k.hash in
  ({hash = k1_hash ; 
    height = k.height ;
    tree = ForkL k.tree},
   {hash = k2_hash ; 
    height = k.height ;
    tree = ForkR k.tree})     
    
let rec forkn = function
  | n when n < 0 -> failwith "cannot fork a negative number of times"
  | 0 -> fun _ -> []
  | n -> fun k -> let k1,k2 = fork k in k1 :: (forkn (n-1) k2)
                                     
let hash seed x = Hashtbl.seeded_hash seed x.hash
let compare = Pervasives.compare
let equal x y = x == y || (compare x y = 0)
let sanitize x = x
  
let height x = x.height
  
  (* Flips a coin with a certain bias, higher biases mean more
     likihood of true, and lesser liklihood of false.  *)
let flip ?bias:(b=2) k =
  assert ( b >= 2 ) ;
  let h : int = Hashtbl.hash k in
    (* XXX -- this could be better; we only use the LSBs of the hash. *)
  let outcome = ( h mod b ) <> 0 in
  outcome
