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
      Or_error.try_with_join ~backtrace:true (fun () ->
        let result = f elt in
        Ppx_quick_test_runtime_lib.assert_no_expect_test_trailing_output
          here_pos
          M.sexp_of_t
          elt;
        result))
;;

include Ppx_quick_test_runtime_lib.Make (struct
    module IO = Monad.Ident

    let run = run
  end)
