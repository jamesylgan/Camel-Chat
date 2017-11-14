
(* Part of this module type is taken directly from the Lab 7 solutions. *)
module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings
   * as [d] and also a mapping from [k] to [v].  If [k] was already mapped
   * in [d], that mapping is replaced in [d'] with the new mpaping. *)
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t

  (* [get k d] returns the value associated with [k] in [d].
   * raises:  [Not_found] if [k] is not mapped to any value in [d].*)
  val get : 'k -> ('k, 'v) t -> 'v

  (* [remove k d] produces a new dictionary [d'] with the same mappings as [d],
   * excluding the mapping whose key is [k]. If [k] was not mappend in [d], then
   * [remove k d] is just [d]. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

end
