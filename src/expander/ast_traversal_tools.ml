open! Core
open Ppxlib

let ghost_all_locs =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let is_quickcheck_test_failure_expression expression =
  let get_payload_from_expect_expression expression =
    match expression.pexp_desc with
    | Pexp_extension ({ txt = "expect"; _ }, payload) -> Some payload
    | _ -> None
  in
  let get_expression_from_payload = function
    | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Some expr
    | _ -> None
  in
  let get_string_from_expression expression =
    match expression.pexp_desc with
    | Pexp_constant (Pconst_string (str, _, _)) -> Some str
    | _ -> None
  in
  expression
  |> get_payload_from_expect_expression
  |> Option.bind ~f:get_expression_from_payload
  |> Option.bind ~f:get_string_from_expression
  |> Option.value_map
       ~default:false
       ~f:(String.is_substring ~substring:Ppx_quick_test_common.test_failed_message)
;;

let find_and_remove_all_quickcheck_failed_expect_extensions =
  object
    inherit [expression Reversed_list.t] Ast_traverse.fold_map as super

    method! expression expression quickcheck_failed_expect_expressions =
      match expression.pexp_desc with
      | Pexp_sequence (e, inner) when is_quickcheck_test_failure_expression inner ->
        super#expression e (inner :: quickcheck_failed_expect_expressions)
      | _ ->
        (match is_quickcheck_test_failure_expression expression with
         | false -> super#expression expression quickcheck_failed_expect_expressions
         | true ->
           let loc = expression.pexp_loc in
           let unit_expr_to_replace_expect = [%expr ()] in
           let quickcheck_failed_expect_expressions =
             Reversed_list.(expression :: quickcheck_failed_expect_expressions)
           in
           super#expression
             unit_expr_to_replace_expect
             quickcheck_failed_expect_expressions)
  end
;;
