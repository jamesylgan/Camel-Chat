
module type Dictionary = sig
  type ('k, 'v) t
  val empty : ('k, 'v) t
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
  val get : 'k -> ('k, 'v) t -> 'v
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
end

module ListDict = struct
  type ('k, 'v) t = ('k * 'v) list

  let empty = []

  let remove = List.remove_assoc

  let insert k v d = (k, v) :: (if List.mem_assoc k d then remove k d else d)

  let get k d = List.assoc
end
