open! Core
open Ppxlib

type t =
  { loc : location
  ; name : string option
  ; parameters : (pattern * core_type) list
  ; body : expression
  ; quickcheck_failed_expects : expression list
  ; attributes : Quick_test_attributes.t
  ; value_binding_attributes : attributes
  }

let parse_name_from_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_any -> None
  | Ppat_constant (Pconst_string (name, _, _)) -> Some name
  | _ ->
    Location.raise_errorf
      ~loc:pattern.ppat_loc
      {|"ppx_quick_test" expects the test name to be a string or wildcard. For example:
{[
    let%%quick_test "my test" = fun (x : int) -> assert (x * x >= 0)
]}
or
{[
    let%%quick_test _ = fun (x : int) -> assert (x * x >= 0)
]}
|}
;;

let parse_parameters_and_body_from_expression expression
  : (pattern * core_type) list * expression
  =
  let parse_pattern_and_type_from_argument pattern =
    match pattern.ppat_desc with
    | Ppat_constraint (parameter, core_type) -> Some (parameter, core_type)
    | _ ->
      Location.raise_errorf
        ~loc:pattern.ppat_loc
        {|"ppx_quick_test" expects the parameter to be a constraint in the form (PARAM_NAME : TYPE). For example:
{[
    let%%quick_test "my test" = fun (x : int) -> assert (x * x >= 0)
]}
|}
  in
  let parse_one_parameter expression : (pattern * core_type) option * expression =
    match expression.pexp_desc with
    | Pexp_function (param :: rest_params, None, body) -> (
      match param.pparam_desc with
      | Pparam_val (Nolabel, None, pattern) ->
        let rest_of_expression =
          match rest_params, body with
          | [], Pfunction_body body_expr -> body_expr
          | _ ->
            { expression with
              pexp_desc = Pexp_function (rest_params, None, body)
            }
        in
        parse_pattern_and_type_from_argument pattern, rest_of_expression
      | _ -> None, expression)
    | _ -> None, expression
  in
  let parse_all_parameters expression =
    let rec loop rev_param_list expression =
      match parse_one_parameter expression with
      | None, rest_of_expression -> rev_param_list, rest_of_expression
      | Some param, rest_of_expression ->
        loop Reversed_list.(param :: rev_param_list) rest_of_expression
    in
    let param_list, rest_of_expression = loop Reversed_list.[] expression in
    match param_list with
    | [] ->
      Location.raise_errorf
        ~loc:expression.pexp_loc
        {|
"ppx_quick_test" expects a function with at least one parameter. For example:
{[
    let%%quick_test "my test" = fun (x : int) -> assert (x * x >= 0)
]}
|}
    | param_list -> Reversed_list.rev param_list, rest_of_expression
  in
  parse_all_parameters expression
;;

let parse_quickcheck_failed_expects_from_body body : expression * expression list =
  let body, rev_expects =
    Ast_traversal_tools.find_and_remove_all_quickcheck_failed_expect_extensions#expression
      body
      Reversed_list.[]
  in
  body, Reversed_list.rev rev_expects
;;

let parse ~loc ~pattern ~expression ~value_binding_attributes =
  let name = parse_name_from_pattern pattern in
  let parameters, body = parse_parameters_and_body_from_expression expression in
  let { Quick_test_attributes.Parse_result.new_pattern = _
      ; new_parameters = parameters
      ; attributes
      }
    =
    Quick_test_attributes.parse ~pattern ~parameters
  in
  let body, quickcheck_failed_expects = parse_quickcheck_failed_expects_from_body body in
  { loc
  ; name
  ; parameters
  ; body
  ; quickcheck_failed_expects
  ; attributes
  ; value_binding_attributes
  }
;;

let expand_to_value_binding_pattern loc name =
  let open (val Ast_builder.make loc) in
  Option.value_map
    name
    ~f:(fun name -> ppat_constant (Pconst_string (name, loc, None)))
    ~default:ppat_any
;;

let expand_parameters_to_input_type loc parameters =
  let open (val Ast_builder.make loc) in
  ptyp_tuple (List.map parameters ~f:snd)
;;

let expand_parameters_to_pattern loc parameters =
  let open (val Ast_builder.make loc) in
  ppat_tuple
    (List.map parameters ~f:(fun (pattern, core_type) ->
       ppat_constraint pattern core_type))
;;

let expand_quickcheck_call loc parameters body attributes ~has_errors =
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let input_type = expand_parameters_to_input_type loc parameters in
  let parameters_pattern = expand_parameters_to_pattern loc parameters in
  let here_expression = hide_expression (Ppx_here_expander.lift_position ~loc) in
  let filename_expression =
    hide_expression (estring (Ppx_here_expander.expand_filename loc.loc_start.pos_fname))
  in
  let quick_test_args =
    let attribute_args =
      Quick_test_attributes.expand_to_args attributes ~loc ~input_type
    in
    let other_args =
      Quick_test_parameter.
        [ Here_pos, here_expression
        ; Filename, filename_expression
        ; Sexp_of, hide_expression [%expr [%sexp_of: [%t input_type]]]
        ; (Error_already_placed, if has_errors then [%expr true] else [%expr false])
        ; ( Property_function
          , [%expr fun [%p parameters_pattern] -> [%e focus_expression body]] )
        ]
    in
    attribute_args @ other_args
    |> List.map ~f:(fun (param, expr) -> Quick_test_parameter.to_arg_label param, expr)
  in
  let run_quick_test_function_identifier =
    hide_expression [%expr Ppx_quick_test_core.run_quick_test]
  in
  pexp_apply run_quick_test_function_identifier quick_test_args
;;

let expand_to_value_binding_expression
  loc
  parameters
  body
  quickcheck_failed_expects
  attributes
  =
  let open (val Ast_builder.make loc) in
  let quickcheck_call =
    expand_quickcheck_call
      loc
      parameters
      body
      attributes
      ~has_errors:(not (List.is_empty quickcheck_failed_expects))
  in
  let quickcheck_failed_expects =
    List.map quickcheck_failed_expects ~f:Ast_traversal_tools.ghost_all_locs#expression
  in
  match quickcheck_failed_expects with
  | [] -> quickcheck_call
  | hd :: tl ->
    [%expr
      Ppx_quick_test_core.map [%e quickcheck_call] ~f:(fun () ->
        [%e List.fold ~init:hd tl ~f:pexp_sequence])]
;;

let expand_to_value_binding loc name parameters body quickcheck_failed_expects attributes =
  let open (val Ast_builder.make loc) in
  let pat = expand_to_value_binding_pattern loc name in
  let expr =
    expand_to_value_binding_expression
      loc
      parameters
      body
      quickcheck_failed_expects
      attributes
  in
  value_binding ~pat ~expr
;;

(*
   The expand function roughly generates the following:

   {[
     let%expect_test "TEST_NAME" =
       Ppx_quick_test_core.run_quick_test
         ~here_pos:[%here]
         ~corrections:__ppx_quick_test_file_corrections
         (module struct
           type t = (TYPE1 * ... * TYPEN) [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (((PARAM1 : TYPE1), ... (PARAMN : TYPEN))) -> BODY);
       [%expect "quickcheck: test failed" ...]
     [@@value_binding_attributes]
   ]}

   Notes:
   - The "TEST_NAME" could also be a wildcard [ _ ]
   - [%here] is actually a location pointing to let%quick_test top level location
   - The [%expect "quickcheck: test failed"] only exist if they existed in the body.
     Without this, if an error occurs within the quick test, the build system would
     place a [%expect "quickcheck: test failed"] in the source code after the body.
     This would be interpreted as being inside the body, which causes an expect block
     that changes each time you accept it.
   - We simply pass the value binding attributes through, which allows other ppxs to use
     them further down the line (e.g. [@@expect.uncaught_exn] interacting with let%expect_test)
   - We also pass user provided attributes as arguments to run_quick_test
*)
let expand
  { loc
  ; name
  ; parameters
  ; body
  ; quickcheck_failed_expects
  ; attributes
  ; value_binding_attributes
  }
  =
  let open (val Ast_builder.make loc) in
  let value_binding =
    expand_to_value_binding loc name parameters body quickcheck_failed_expects attributes
  in
  let value_binding_with_attributes =
    { value_binding with pvb_attributes = value_binding_attributes }
  in
  let let_structure_item = pstr_value Nonrecursive [ value_binding_with_attributes ] in
  pstr_extension (Loc.make ~loc "expect_test", PStr [ let_structure_item ]) []
;;
