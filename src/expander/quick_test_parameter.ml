open! Core
open Ppxlib

type t =
  | Config
  | Cr
  | Examples
  | Hide_positions
  | Sexp_examples
  | Generator
  | Shrinker
  | Here_pos
  | Filename
  | Sexp_of
  | Property_function
  | Error_already_placed

let to_arg_label = function
  | Config -> Labelled "config"
  | Cr -> Labelled "cr"
  | Examples -> Labelled "examples"
  | Hide_positions -> Labelled "hide_positions"
  | Sexp_examples -> Labelled "sexp_examples"
  | Generator -> Labelled "generator"
  | Shrinker -> Labelled "shrinker"
  | Here_pos -> Labelled "here_pos"
  | Filename -> Labelled "filename"
  | Sexp_of -> Labelled "sexp_of"
  | Error_already_placed -> Labelled "error_already_placed"
  | Property_function -> Nolabel
;;
