open! Core
open Ppxlib
open Ppx_quick_test_expander

let pattern () =
  let open Ast_pattern in
  pstr (pstr_value nonrecursive (__ ^:: nil) ^:: nil)
;;

let quick_test =
  Extension.V3.declare
    "quick_test"
    Structure_item
    (pattern ())
    (fun ~ctxt value_binding ->
    let expression = value_binding.pvb_expr in
    let pattern = value_binding.pvb_pat in
    let attributes = value_binding.pvb_attributes in
    let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
    let loc = { loc with loc_ghost = true } in
    expand ~loc ~pattern ~expression ~attributes)
;;

let enclose_impl = function
  | None -> [], []
  | Some loc ->
    let loc = { loc with loc_ghost = true } in
    Ppx_quick_test_expander.enclose_impl loc
;;

let () =
  Driver.register_transformation
    "quick_test"
    ~rules:[ Context_free.Rule.extension quick_test ]
    ~enclose_impl
;;
