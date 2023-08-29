open! Core

let run
      (type a)
      ~here_pos
      ?config
      ?cr
      ~examples
      ?hide_positions
      (module M : Base_quickcheck.Test.S with type t = a)
      ~f
  =
  (* Adapted from Expect_test_helpers_base.quickcheck_m *)
  Base_quickcheck.Test.result
    ?config
    ~examples
    (module M)
    ~f:(fun elt ->
      let crs = Queue.create () in
      (* We set [on_print_cr] to accumulate CRs in [crs]; it affects both [f elt] as
         well as our call to [require_does_not_raise]. *)
      Ref.set_temporarily
        Expect_test_helpers_base.on_print_cr
        (Queue.enqueue crs)
        ~f:(fun () ->
          Expect_test_helpers_base.require_does_not_raise
            here_pos
            ?cr
            ?hide_positions
            (fun () ->
               f elt;
               Ppx_quick_test_runtime_lib.assert_no_expect_test_trailing_output
                 here_pos
                 M.sexp_of_t
                 elt));
      if Queue.is_empty crs then Ok () else Error (Queue.to_list crs))
;;

include Ppx_quick_test_runtime_lib.Make (struct
    module IO = Monad.Ident

    let run = run
  end)
