open! Core

module One : sig
  type ('a : value_or_null) t =
    { sexp_of : 'a -> Sexp.t
    ; of_sexp : (Sexp.t -> 'a) option
    ; generator : 'a Quickcheck.Generator.t
    ; shrinker : 'a Quickcheck.Shrinker.t
    }
end

(** Heterogenous list representing multiple parameters. *)
type ('tup, 'fn, 'res) many =
  | [] : (unit, 'res, 'res) many
  | ( :: ) :
      ('a : value_or_null) 'tup 'fn 'res.
      'a One.t * ('tup, 'fn, 'res) many
      -> ('a * 'tup, 'a -> 'fn, 'res) many

(** [('tup, 'fn, 'res) t] represents one or more ['a One.t] parameters of a quick-test
    predicate. ['tup] is the list of parameters represented as a nested tuple, like
    [(x, (y, (z, ())))] (or just [x] if there's only one parameter). ['fn] is a function
    type that accepts each of the ['a One.t]s (curried), and returns ['res]. *)
type ('tup : value_or_null, 'fn, 'res) t =
  | One : ('a : value_or_null) 'res. 'a One.t -> ('a, 'a -> 'res, 'res) t
  | Many : ('tup, 'fn, 'res) many -> ('tup, 'fn, 'res) t

(** Applies the curried ['fn] to the tuple of arguments ['tup]. *)
val call : ('tup : value_or_null) 'fn 'res. ('tup, 'fn, 'res) t -> 'tup -> 'fn -> 'res

(** Sexp-converters and quickcheck values for ['tup]s. *)

val sexp_of_tup
  : ('tup : value_or_null) 'fn 'res.
  ('tup, 'fn, 'res) t -> ('tup -> Sexp.t) Staged.t

val tup_of_sexp
  : ('tup : value_or_null) 'fn 'res.
  ('tup, 'fn, 'res) t -> (Sexp.t -> 'tup) Staged.t

val tup_generator
  : ('tup : value_or_null) 'fn 'res.
  ('tup, 'fn, 'res) t -> 'tup Quickcheck.Generator.t

val tup_shrinker
  : ('tup : value_or_null) 'fn 'res.
  ('tup, 'fn, 'res) t -> 'tup Quickcheck.Shrinker.t
