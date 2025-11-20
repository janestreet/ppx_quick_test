open! Core
open Ppxlib

module String_constant = struct
  type t =
    { string_value : string
    ; location : location
    ; delimiter : string option
    }
end

module Sexp_examples = struct
  type t =
    | Provided of
        { string_constants : String_constant.t list
        ; expression_placement_cnum : int
        }
    | NotProvided
end

module Shrinker = struct
  type t =
    | Custom of expression
    | Default of core_type
end

module Generator = struct
  type t =
    | Custom of expression
    | Default of core_type
end

module Attribute_name = struct
  (* The names of the attributes that are written by the user in the form
     [@parameter_name] *)
  type t =
    | Config
    | Trials
    | Cr
    | Examples
    | Hide_positions
    | Generator
    | Shrinker
    | Remember_failures
    | Remember_failures_ignore
    | Tags

  let to_string t =
    let prefix = "quick_test" in
    let body =
      match t with
      | Config -> "config"
      | Trials -> "trials"
      | Cr -> "cr"
      | Examples -> "examples"
      | Hide_positions -> "hide_positions"
      | Generator -> "generator"
      | Shrinker -> "shrinker"
      | Remember_failures -> "remember_failures"
      | Remember_failures_ignore -> "remember_failures.ignore"
      | Tags -> "tags"
    in
    [%string "%{prefix}.%{body}"]
  ;;

  let pass_throughs =
    [ Config, Quick_test_parameter.Config
    ; Trials, Quick_test_parameter.Trials
    ; Cr, Quick_test_parameter.Cr
    ; Hide_positions, Quick_test_parameter.Hide_positions
    ; Examples, Quick_test_parameter.Examples
    ]
  ;;

  let test_scoped =
    [ Config
    ; Trials
    ; Cr
    ; Examples
    ; Remember_failures
    ; Remember_failures_ignore
    ; Hide_positions
    ; Tags
    ]
  ;;

  let type_scoped = [ Generator; Shrinker ]
end

type t =
  { pass_through_attrs : (Quick_test_parameter.t * expression) list
  ; sexp_examples : Sexp_examples.t
  ; generators : Generator.t list
  ; shrinkers : Shrinker.t list
  ; tags : expression option
  }
[@@deriving fields ~fields ~getters]

module Parse_result = struct
  type nonrec t =
    { new_pattern : pattern
    ; new_parameters : (pattern * core_type) list
    ; attributes : t
    }
end

let declare_single_expr_attribute (name : Attribute_name.t) ~context =
  Attribute.declare
    (Attribute_name.to_string name)
    context
    Ast_pattern.(single_expr_payload __)
    Fn.id
;;

let pass_through_attributes =
  Attribute_name.pass_throughs
  |> List.map ~f:(fun (attribute_name, parameter_name) ->
    ( parameter_name
    , declare_single_expr_attribute attribute_name ~context:Attribute.Context.Pattern ))
;;

let sexp_examples_attribute =
  Attribute.declare_with_attr_loc
    (Attribute_name.to_string Remember_failures)
    Attribute.Context.Pattern
    Ast_pattern.(alt_option (single_expr_payload __) (pstr nil))
    (fun ~attr_loc e -> attr_loc, e)
;;

let tags_attribute =
  declare_single_expr_attribute Attribute_name.Tags ~context:Attribute.Context.Pattern
;;

let generator_attribute =
  declare_single_expr_attribute
    Attribute_name.Generator
    ~context:Attribute.Context.Core_type
;;

let shrinker_attribute =
  declare_single_expr_attribute
    Attribute_name.Shrinker
    ~context:Attribute.Context.Core_type
;;

let assert_no_unused_attributes pattern =
  let unused_attributes = Attribute.collect_unused_attributes_errors#pattern pattern [] in
  match unused_attributes with
  | [] -> ()
  | error :: _ ->
    let format_names names =
      names
      |> List.map ~f:Attribute_name.to_string
      |> List.map ~f:(Format.sprintf "* [@%s]")
      |> String.concat ~sep:"\n"
    in
    let message =
      [%string
        {|%{Location.Error.message error}

"ppx_quick_test" found an unexpected attribute.

Supported test-scoped attributes:
%{format_names Attribute_name.test_scoped}

Supported type-scoped attributes:
%{format_names Attribute_name.type_scoped}

For example:
  let%quick_test _ [@quick_test.hide_positions true]
    = fun (x : int [@quick_test.generator Base_quickcheck.Generator.int_uniform]) -> assert ( * x >= 0)
|}]
    in
    let error = Location.Error.set_message error message in
    Location.Error.raise error
;;

let parse_attribute_from_context ~context ~attribute =
  Attribute.consume_res attribute context
  |> Result.map ~f:(function
    | Some (new_context, expr) -> new_context, Some expr
    | None -> context, None)
  |> function
  | Ok res -> res
  | Error error_list ->
    let name = Attribute.name attribute in
    let error = Stdppx.NonEmptyList.hd error_list in
    let message =
      [%string
        {|"ppx_quick_test" found incorrect use of attribute `%{name}': %{Location.Error.message error}. Example of correct usage:
{[
   let%%quick_test "my test" [@quick_test.cr CR.CR_someday] = fun (x : int) -> assert (x * x >= 0)
]} |}]
    in
    let error = Location.Error.set_message error message in
    Location.Error.raise error
;;

let parse_pass_through_attributes_from_pattern pattern =
  let pattern, attributes =
    List.fold_map
      pass_through_attributes
      ~init:pattern
      ~f:(fun pattern (param_name, attribute) ->
        let pattern, expr = parse_attribute_from_context ~context:pattern ~attribute in
        pattern, Option.map expr ~f:(fun expr -> param_name, expr))
  in
  let attributes = List.filter_opt attributes in
  pattern, attributes
;;

let parse_string_constant_from_expression expression =
  match expression.pexp_desc with
  | Pexp_constant (Pconst_string (contents, loc, delimiter)) ->
    { String_constant.string_value = contents; location = loc; delimiter }
  | _ ->
    Location.raise_errorf
      ~loc:expression.pexp_loc
      {|"ppx_quick_test" expected this expression to be a string literal constant|}
;;

let parse_expression_from_unlabeled_argument argument =
  match fst argument with
  | Nolabel -> snd argument
  | _ ->
    Location.raise_errorf
      ~loc:(snd argument).pexp_loc
      {|"ppx_quick_test" expected this argument to unlabeled|}
;;

let parse_string_constant_list_from_expression expr =
  Option.value_map ~default:[] expr ~f:(fun expr ->
    match expr.pexp_desc with
    | Pexp_apply (first, rest) ->
      let exprs = first :: List.map rest ~f:parse_expression_from_unlabeled_argument in
      List.map exprs ~f:parse_string_constant_from_expression
    | _ -> [ parse_string_constant_from_expression expr ])
;;

let parse_sexp_examples_attribute pattern =
  let pattern, sexp_examples_payload =
    parse_attribute_from_context ~context:pattern ~attribute:sexp_examples_attribute
  in
  let sexp_examples =
    match sexp_examples_payload with
    | None -> Sexp_examples.NotProvided
    | Some (attr_location, expr) ->
      Sexp_examples.Provided
        { string_constants = parse_string_constant_list_from_expression expr
        ; expression_placement_cnum =
            (match expr with
             | Some expr -> expr.pexp_loc.loc_end.pos_cnum
             | None -> attr_location.loc_end.pos_cnum - 1)
        }
  in
  pattern, sexp_examples
;;

let parse_tags_attribute pattern =
  parse_attribute_from_context ~context:pattern ~attribute:tags_attribute
;;

let parse_generators_and_shrinkers parameters =
  let resolve_generator ~default ~generator_payload =
    match generator_payload with
    | None -> Generator.Default default
    | Some expr -> Generator.Custom expr
  in
  let resolve_shrinker ~default ~shrinker_payload =
    match shrinker_payload with
    | None -> Shrinker.Default default
    | Some expr -> Shrinker.Custom expr
  in
  let parse_generator_and_shrinker (pattern, type_) =
    let type_, generator_payload =
      parse_attribute_from_context ~context:type_ ~attribute:generator_attribute
    in
    let type_, shrinker_payload =
      parse_attribute_from_context ~context:type_ ~attribute:shrinker_attribute
    in
    let generator = resolve_generator ~default:type_ ~generator_payload in
    let shrinker = resolve_shrinker ~default:type_ ~shrinker_payload in
    (pattern, type_), generator, shrinker
  in
  parameters |> List.map ~f:parse_generator_and_shrinker |> List.unzip3
;;

let parse ~pattern ~parameters =
  let pattern, pass_through_attrs = parse_pass_through_attributes_from_pattern pattern in
  let pattern, sexp_examples = parse_sexp_examples_attribute pattern in
  let pattern, tags = parse_tags_attribute pattern in
  let parameters, generators, shrinkers = parse_generators_and_shrinkers parameters in
  assert_no_unused_attributes pattern;
  let attributes = { pass_through_attrs; sexp_examples; generators; shrinkers; tags } in
  { Parse_result.new_pattern = pattern; new_parameters = parameters; attributes }
;;

let expand_string_constant_to_expression
  { String_constant.string_value; location = loc; delimiter }
  =
  let open (val Ast_builder.make loc) in
  pexp_constant (Pconst_string (string_value, loc, delimiter))
;;

let create_list_expression expr_list ~loc =
  let open (val Ast_builder.make loc) in
  let list_expr =
    List.fold_right expr_list ~init:[%expr []] ~f:(fun list_elem acc ->
      [%expr [%e list_elem] :: [%e acc]])
  in
  list_expr
;;

let create_int_expression n ~loc =
  let open (val Ast_builder.make loc) in
  Pconst_integer (Int.to_string n, None) |> pexp_constant
;;

let expand_not_provided_sexp_examples_expression ~loc =
  let open (val Ast_builder.make loc) in
  [%expr Ppx_quick_test_runtime_lib.Sexp_examples.NotProvided]
;;

let expand_provided_sexp_examples_expression
  ~loc
  ~input_type
  ~string_constants
  ~expression_placement_cnum
  =
  let open (val Ast_builder.make loc) in
  let string_constant_exprs =
    List.map string_constants ~f:expand_string_constant_to_expression
  in
  [%expr
    Ppx_quick_test_runtime_lib.Sexp_examples.Provided
      { sexp_strings = [%e create_list_expression ~loc string_constant_exprs]
      ; of_sexp = [%of_sexp: [%t input_type]]
      ; expression_placement_cnum =
          [%e create_int_expression ~loc expression_placement_cnum]
      }]
;;

let expand_sexp_examples_argument sexp_examples ~loc ~input_type =
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let expr =
    match sexp_examples with
    | Sexp_examples.NotProvided -> expand_not_provided_sexp_examples_expression ~loc
    | Sexp_examples.Provided
        { string_constants : String_constant.t list; expression_placement_cnum : int } ->
      expand_provided_sexp_examples_expression
        ~loc
        ~input_type
        ~string_constants
        ~expression_placement_cnum
  in
  Quick_test_parameter.Sexp_examples, hide_expression expr
;;

let expand_generator_argument (generators : Generator.t list) ~loc =
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let generator_type =
    generators
    |> List.map ~f:(function
      | Default type_ -> type_
      | Custom expr -> [%type: [%custom [%e expr]]])
    |> ptyp_tuple
  in
  let expr = hide_expression [%expr [%quickcheck.generator: [%t generator_type]]] in
  Quick_test_parameter.Generator, expr
;;

let expand_shrinker_argument (shrinkers : Shrinker.t list) ~loc =
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let shrinkers_type =
    shrinkers
    |> List.map ~f:(function
      | Default type_ -> type_
      | Custom expr -> [%type: [%custom [%e expr]]])
    |> ptyp_tuple
  in
  let expr = hide_expression [%expr [%quickcheck.shrinker: [%t shrinkers_type]]] in
  Quick_test_parameter.Shrinker, expr
;;

let expand_to_args t ~loc ~input_type =
  let sexp_examples_arg =
    expand_sexp_examples_argument t.sexp_examples ~loc ~input_type
  in
  let generator_arg = expand_generator_argument t.generators ~loc in
  let shrinker_arg = expand_shrinker_argument t.shrinkers ~loc in
  let args = generator_arg :: shrinker_arg :: sexp_examples_arg :: t.pass_through_attrs in
  args
;;
