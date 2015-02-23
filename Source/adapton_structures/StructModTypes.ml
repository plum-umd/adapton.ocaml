open Adapton_core
open Primitives
open GrifolaType
module Types = AdaptonTypes
module Statistics = AdaptonStatistics

module Key = Key

module type SeqType
= sig

  type data_t
  type list_t
  type rope_t
  type tree_t

  module Name : GrifolaType.NameType

  module LArt : ArtType
  module RArt : ArtType
  module TArt : ArtType

  val mut_elms_of_list : 
    Key.t ->
    'a list ->
    ('a -> DatType.t) ->
    ('a -> Key.t) ->
    int ->
    ArtType.t

  val insert_elm : LArt.t -> data_t -> (Name.t * LArt.t) option -> LArt.t
  val delete_elm : LArt.t -> (data_t * list_t)
 
  val next_art : LArt.t -> LArt.t option
  val take : list_t -> int -> data_t list

  val list_is_empty : list_t -> bool
  val list_append : list_t -> list_t -> list_t

  val list_of_tree : tree_t -> list_t -> list_t
  val tree_of_list : list_t -> tree_t

  val rope_of_list : list_t -> rope_t
  val list_of_rope : rope_t -> list_t -> list_t

  val rope_length : rope_t -> int
  val rope_nth : rope_t -> int -> data_t option

  val list_reverse : list_t -> list_t -> list_t
  val tree_reverse : tree_t -> tree_t
  val rope_reverse : rope_t -> rope_t

  val list_reduce : Name.t -> (data_t -> data_t -> data_t) -> list_t -> data_t option
  val tree_reduce : Name.t -> (data_t -> data_t -> data_t) -> tree_t -> data_t option
  val rope_reduce : Name.t -> (data_t -> data_t -> data_t) -> rope_t -> data_t option

  val rope_median : rope_t -> data_t option

  val list_mergesort : Name.t -> (data_t -> data_t -> int) -> list_t -> list_t
  val rope_mergesort : Name.t -> (data_t -> data_t -> int) -> rope_t -> list_t
  val list_to_rope_mergesort : Name.t -> (data_t -> data_t -> int) -> list_t -> list_t

end