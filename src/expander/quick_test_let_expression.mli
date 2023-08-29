open Ppxlib

(** Represents an instance of a let%quick_test expression *)
type t

(** Creates a [t] given a the [location], [pattern], and [expression] that
    exist in the AST in the form [%%quick_test let PATTERN = EXPRESSION],
    which comes from the [Pstr_value structure_item_desc] associated with the let.

    Note: after code expansion the function arguments exist in the [expression] rather
    than the [pattern]. *)
val parse
  :  loc:location
  -> pattern:pattern
  -> expression:expression
  -> value_binding_attributes:attributes
  -> t

(** Given a [t], creates the structure item representing the equivalent let%expect_test. *)
val expand : t -> structure_item
