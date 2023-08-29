open! Core
open! Async_kernel

module Ppx_quick_test_core : Ppx_quick_test_runtime_lib.S with module IO := Deferred
(** @inline *)
