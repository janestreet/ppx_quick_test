open! Core
open Async_kernel

let run
  (type a)
  ~here_pos
  ?(config : Base_quickcheck.Test.Config.t option)
  ?(trials : int option)
  ~examples
  (module M : Base_quickcheck.Test.S with type t = a)
  ~(f : a -> unit Or_error.t Deferred.t)
  =
  Printexc.record_backtrace true;
  let seed =
    Option.map config ~f:(fun config ->
      match config.Base_quickcheck.Test.Config.seed with
      | Base_quickcheck.Test.Config.Seed.Nondeterministic -> `Nondeterministic
      | Base_quickcheck.Test.Config.Seed.Deterministic seed -> `Deterministic seed)
  in
  let sizes =
    Option.map config ~f:(fun config -> config.Base_quickcheck.Test.Config.sizes)
  in
  let trials =
    Option.first_some trials (Option.map config ~f:(fun config -> config.test_count))
  in
  let shrink_attempts =
    Option.map config ~f:(fun config ->
      `Limit config.Base_quickcheck.Test.Config.shrink_count)
  in
  Async_quickcheck.async_test_or_error
    ?seed
    ?sizes
    ?trials
    ?shrink_attempts
    ~examples
    ~shrinker:M.quickcheck_shrinker
    M.quickcheck_generator
    ~f:(fun elt ->
      let crs = Queue.create () in
      Expect_test_helpers_async.set_temporarily_async
        Expect_test_helpers_base.on_print_cr
        (Queue.enqueue crs)
        ~f:(fun () ->
          let%bind result = f elt in
          Ppx_quick_test_runtime_lib.assert_no_expect_test_trailing_output
            here_pos
            M.sexp_of_t
            elt;
          if Queue.is_empty crs
          then return result
          else
            Deferred.Or_error.error_s
              [%sexp ({ crs } : Ppx_quick_test_runtime_lib.Queue_of_crs_error.t)]))
;;

module Ppx_quick_test_core = Ppx_quick_test_runtime_lib.Make (struct
    module IO = Deferred

    let run = run
  end)
