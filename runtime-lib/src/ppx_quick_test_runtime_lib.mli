open! Core
open Expect_test_helpers_base
module File_corrections = File_corrections
module Sexp_examples = Sexp_examples

module Trailing_output_error : sig
  type t =
    | Ppx_quick_test_trailing_output_error of
        { trailing_output : string
        ; input : Sexp.t
        }
  [@@deriving sexp]

  val of_error : Error.t -> t option
end

val assert_no_expect_test_trailing_output
  :  Lexing.position
  -> ('a -> Sexp.t)
  -> 'a
  -> unit

module type S = sig
  module IO : T1

  val map : 'a IO.t -> f:('a -> 'b) -> 'b IO.t
  val return : 'a -> 'a IO.t

  val run_quick_test
    :  here_pos:Source_code_position.t
    -> ?config:Base_quickcheck.Test.Config.t
         (** default is [Base_quickcheck.Test.default_config] *)
    -> ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?examples:'a list
    -> sexp_examples:'a Sexp_examples.t
    -> sexp_of:('a -> Sexp.t)
    -> generator:'a Base_quickcheck.Generator.t
    -> shrinker:'a Base_quickcheck.Shrinker.t
    -> filename:string
    -> error_already_placed:bool
         (** note: the instance is passed across all quick test calls within a file (using enclose_impl) *)
    -> ('a -> unit IO.t)
    -> unit IO.t
end

module type Arg = sig
  module IO : sig
    include T1

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val return : 'a -> 'a t
  end

  val run
    :  here_pos:Lexing.position
    -> ?config:Base_quickcheck.Test.Config.t
    -> examples:'a list
    -> (module Base_quickcheck.Test.S with type t = 'a)
    -> f:('a -> unit Or_error.t IO.t)
    -> (unit, 'a * Error.t) result IO.t
end

module Make (Arg : Arg) : S with module IO = Arg.IO
