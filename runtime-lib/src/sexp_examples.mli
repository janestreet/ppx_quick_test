open! Core

type 'a t =
  | Provided of
      { sexp_strings : string list
      ; of_sexp : Sexp.t -> 'a
      ; expression_placement_cnum : int
      }
  | NotProvided
  | Disabled

val string_to_ocaml_string : string -> string
val get_parsed_examples : 'a t -> 'a list
val insertion_for_new_example : 'a t -> Sexp.t -> File_corrections.Insertion.t option
