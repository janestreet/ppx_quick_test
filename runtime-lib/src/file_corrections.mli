open! Core

module Insertion : sig
  type t =
    { placement_cnum : int
    ; value : string
    }

  val apply_all : original:string -> t list -> string
end

type t

val create : filename:string -> unit
val add_insertion : filename:string -> Insertion.t -> unit
val make_corrected_file : filename:string -> unit
val disable_due_to_pending_error : filename:string -> unit
