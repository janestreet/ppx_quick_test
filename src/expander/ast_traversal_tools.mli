open! Core
open Ppxlib

val ghost_all_locs : Ast_traverse.map

val find_and_remove_all_quickcheck_failed_expect_extensions
  : expression Reversed_list.t Ast_traverse.fold_map
