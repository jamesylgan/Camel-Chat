
module type Dictionary = sig
  type t
  type key
  type value
  (*replace if key already exists *)
  val insert: t -> key -> value -> t
  val get: t -> key -> value
  val remove: t -> key -> t
end
