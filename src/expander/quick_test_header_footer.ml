open! Core
open! Ppxlib

let expand_impl_header loc =
  let loc = { loc with loc_end = loc.loc_start } in
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let filename_expr =
    estring (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)
  in
  let create_file_corrections_expr =
    [%expr
      Ppx_quick_test_runtime_lib.File_corrections.create ~filename:[%e filename_expr]]
  in
  [%str let () = [%e hide_expression create_file_corrections_expr]]
;;

let expand_impl_footer loc =
  let loc = { loc with loc_start = loc.loc_end } in
  let open (val Ast_builder.make loc) in
  let open Merlin_helpers in
  let filename_expr =
    estring (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)
  in
  let make_corrected_file_function_expression =
    [%expr
      Ppx_quick_test_runtime_lib.File_corrections.make_corrected_file
        ~filename:[%e filename_expr]]
  in
  [%str let () = [%e hide_expression make_corrected_file_function_expression]]
;;

let expand_enclose_impl loc = expand_impl_header loc, expand_impl_footer loc
