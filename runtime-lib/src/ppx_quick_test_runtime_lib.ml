open! Core
open Expect_test_helpers_base
module File_corrections = File_corrections
module Sexp_examples = Sexp_examples

module Trailing_output_error = struct
  type t =
    | Ppx_quick_test_trailing_output_error of
        { trailing_output : string
        ; input : Sexp.t
        }
  [@@deriving sexp]

  module With_backtrace = struct
    type nonrec t = t * string list [@@deriving sexp]
  end

  let of_error error =
    let error_as_sexp = [%sexp (error : Error.t)] in
    Option.first_some
      (Option.try_with (fun () -> [%of_sexp: t] error_as_sexp, []))
      (Option.try_with (fun () -> [%of_sexp: With_backtrace.t] error_as_sexp))
  ;;
end

module Queue_of_crs_error = struct
  type t = { crs : string Queue.t } [@@deriving sexp]

  let of_error error =
    let error_as_sexp = [%sexp (error : Error.t)] in
    Option.try_with (fun () -> [%of_sexp: t] error_as_sexp)
  ;;
end

let assert_no_expect_test_trailing_output position sexp_of input =
  match Expect_test_helpers_base.expect_test_output ~here:position () with
  | "" -> ()
  | trailing_output ->
    let input = sexp_of input in
    let error =
      Trailing_output_error.Ppx_quick_test_trailing_output_error
        { trailing_output; input }
    in
    raise_s [%sexp (error : Trailing_output_error.t)]
;;

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
    -> ?trials:int (** if provided, overrides the number of trials in [config] *)
    -> sexp_examples:'a Sexp_examples.t
    -> sexp_of:('a -> Sexp.t)
    -> generator:'a Base_quickcheck.Generator.t
    -> shrinker:'a Base_quickcheck.Shrinker.t
    -> filename:string
    -> error_already_placed:bool
         (** note: the instance is passed across all quick test calls within a file (using
             enclose_impl) *)
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
    -> ?trials:int
    -> examples:'a list
    -> (module Base_quickcheck.Test.S with type t = 'a)
    -> f:('a -> unit Or_error.t IO.t)
    -> (unit, 'a * Error.t) result IO.t
end

module Make (Arg : Arg) = struct
  module IO = Arg.IO

  let map = IO.map
  let return = IO.return

  let run_quick_test
    (type a)
    ~here_pos
    ?config
    ?cr
    ?hide_positions
    ?(examples = [])
    ?trials
    ~(sexp_examples : a Sexp_examples.t)
    ~sexp_of
    ~generator
    ~shrinker
    ~filename
    ~error_already_placed
    (f : a -> unit IO.t)
    =
    Printexc.record_backtrace true;
    let all_examples = examples @ Sexp_examples.get_parsed_examples sexp_examples in
    let all_examples_set = all_examples |> List.map ~f:sexp_of |> Sexp.Set.of_list in
    IO.map
      (Arg.run
         ~here_pos
         ?config
         ?trials
         ~examples:all_examples
         (module struct
           type t = a

           let sexp_of_t = sexp_of
           let quickcheck_generator = generator
           let quickcheck_shrinker = shrinker
         end)
         ~f:(fun input ->
           match Or_error.try_with ~backtrace:true (fun () -> f input) with
           | Ok x ->
             (* NOTE: This [map] is important in the [Deferred] case as it waits for the
                Deferred effect to finish. Otherwise there is a leak/explosion of pending
                jobs that result in really weird test behaviors. *)
             IO.map x ~f:(fun () -> Ok ())
           | Error error -> IO.return (Error error)))
      ~f:(fun quickcheck_results ->
        Result.iter_error quickcheck_results ~f:(fun (input, output) ->
          (match sexp_of input with
           | input_sexp ->
             (match Set.mem all_examples_set input_sexp with
              | true -> ()
              | false ->
                Sexp_examples.insertion_for_new_example sexp_examples input_sexp
                |> Option.iter ~f:(File_corrections.add_insertion ~filename);
                (match error_already_placed with
                 | true -> ()
                 | false -> File_corrections.disable_due_to_pending_error ~filename));
             print_s
               [%message
                 Ppx_quick_test_common.test_failed_message ~input:(input_sexp : Sexp.t)]
           | exception exn ->
             print_s
               [%message
                 Ppx_quick_test_common.test_failed_message
                   ~exception_encountered_when_converting_input_to_sexp:(exn : exn)]);
          match Trailing_output_error.of_error output with
          | None ->
            (match Queue_of_crs_error.of_error output with
             | None ->
               print_cr ?cr ?hide_positions ~here:here_pos [%sexp (output : Error.t)]
             | Some { crs } -> Queue.iter crs ~f:print_endline)
          | Some
              (Ppx_quick_test_trailing_output_error { trailing_output; input }, backtrace)
            ->
            print_cr
              ?cr
              ?hide_positions
              ~here:here_pos
              [%message
                "Unexpected trailing output, consider adding a trailing [%expect] at the \
                 end of your function body."
                  (input : Sexp.t)];
            print_endline "Trailing output:";
            print_endline trailing_output;
            (match backtrace with
             | [] -> ()
             | _ ->
               print_endline "Backtrace:";
               print_string (String.concat_lines backtrace))))
  ;;
end
