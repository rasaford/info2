
module type Set = sig
  type t
  val to_string : t -> string
end

module type Map = sig
  type key
  type value
  type t

  val empty : t
  val set : key -> value -> t -> t
  val get : key -> t -> value
  val get_opt : key -> t -> value option
  (* val clear : key -> t -> t *)
  val to_string : t -> string
end
