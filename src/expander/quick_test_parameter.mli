open! Core
open Ppxlib

(* The names of the parameters that are passed into
   [Ppx_quick_test_runtime_lib.run_quick_test] *)
type t =
  | Config
  | Trials
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

val to_arg_label : t -> arg_label
