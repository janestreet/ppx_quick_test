open! Core
open Ppxlib

type t

module Parse_result : sig
  type nonrec t =
    { new_pattern : pattern
    ; new_parameters : (pattern * core_type) list
    ; attributes : t
    }
end

val parse : pattern:pattern -> parameters:(pattern * core_type) list -> Parse_result.t

val expand_to_args
  :  t
  -> loc:Location.t
  -> input_type:core_type
  -> (Quick_test_parameter.t * expression) list
