open! Core
open Async_kernel

let run
  (type a)
  ~here_pos
  ?(config : Base_quickcheck.Test.Config.t option)
  ?cr
  ~examples
  ?hide_positions
  (module M : Base_quickcheck.Test.S with type t = a)
  ~(f : a -> unit Deferred.t)
  =
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
    Option.map config ~f:(fun config -> config.Base_quickcheck.Test.Config.test_count)
  in
  let shrink_attempts =
    Option.map config ~f:(fun config ->
      `Limit config.Base_quickcheck.Test.Config.shrink_count)
  in
  let ivar = Ivar.create () in
  let%bind () =
    let%bind () =
      Async_quickcheck.async_test
        ?seed
        ?sizes
        ?trials
        ?shrink_attempts
        ~sexp_of:M.sexp_of_t
        ~examples
        ~shrinker:M.quickcheck_shrinker
        M.quickcheck_generator
        ~f:(fun elt ->
        let crs = Queue.create () in
        let%bind () =
          Expect_test_helpers_async.set_temporarily_async
            Expect_test_helpers_base.on_print_cr
            (Queue.enqueue crs)
            ~f:(fun () ->
            Expect_test_helpers_async.require_does_not_raise_async
              here_pos
              ?cr
              ?hide_positions
              (fun () ->
              let%bind () = f elt in
              Ppx_quick_test_runtime_lib.assert_no_expect_test_trailing_output
                here_pos
                M.sexp_of_t
                elt;
              return ()))
        in
        (match Queue.is_empty crs with
         | true -> ()
         | false -> Ivar.fill_if_empty ivar (Error (elt, Queue.to_list crs)));
        return ())
    in
    Ivar.fill_if_empty ivar (Ok ());
    return ()
  in
  Ivar.read ivar
;;

module Ppx_quick_test_core = Ppx_quick_test_runtime_lib.Make (struct
  module IO = Deferred

  let run = run
end)
