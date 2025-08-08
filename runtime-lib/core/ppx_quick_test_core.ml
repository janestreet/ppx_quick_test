open! Core

let run
  (type a)
  ~here_pos
  ?config
  ?trials
  ~examples
  (module M : Base_quickcheck.Test.S with type t = a)
  ~(f : a -> unit Or_error.t)
  =
  let config = Option.value config ~default:Base_quickcheck.Test.default_config in
  Base_quickcheck.Test.result
    ~config:{ config with test_count = Option.value trials ~default:config.test_count }
    ~examples
    (module M)
    ~f:(fun elt ->
      let crs = Atomic.make [] in
      Dynamic.with_temporarily
        Expect_test_helpers_base.on_print_cr
        (fun cr -> Atomic.update crs ~pure_f:(fun crs -> cr :: crs))
        ~f:(fun () ->
          Or_error.try_with_join ~backtrace:true (fun () ->
            let result = f elt in
            Ppx_quick_test_runtime_lib.assert_no_expect_test_trailing_output
              here_pos
              M.sexp_of_t
              elt;
            let crs = Atomic.get crs |> List.rev in
            if List.is_empty crs
            then result
            else
              Or_error.error_s
                [%sexp ({ crs } : Ppx_quick_test_runtime_lib.List_of_crs_error.t)])))
;;

include Ppx_quick_test_runtime_lib.Make (struct
    module IO = Monad.Ident

    let run = run
  end)
