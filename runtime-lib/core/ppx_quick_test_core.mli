open! Core

include Ppx_quick_test_runtime_lib.S with module IO := Monad.Ident (** @inline *)
