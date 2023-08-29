open! Core

type 'a t =
  | Provided of
      { sexp_strings : string list
      ; of_sexp : Sexp.t -> 'a
      ; expression_placement_cnum : int
      }
  | NotProvided
  | Disabled

let choose_tag_for str =
  let rec loop len =
    let tag = String.make len 'x' in
    let delimiters = [ sprintf "{%s|" tag; sprintf "|%s}" tag ] in
    if List.exists delimiters ~f:(fun delim -> String.is_substring str ~substring:delim)
    then loop (len + 1)
    else tag
  in
  loop 0
;;

let string_to_ocaml_string str =
  let tag = choose_tag_for str in
  [%string "{%{tag}|%{str}|%{tag}}"]
;;

let value_of_string ~of_sexp sexp_string =
  let get_value_or_error ~of_sexp sexp_string =
    let%bind.Result sexp =
      try Ok (Sexp.of_string sexp_string) with
      | e ->
        Error
          (Error.create_s
             [%message
               {|"ppx_quick_test" was unable to convert a sexp example string into a sexp: |}
                 ~sexp_example_string:(sexp_string : string)
                 ~raised_exception:(e : exn)])
    in
    try Ok (of_sexp sexp) with
    | e ->
      Error
        (Error.create_s
           [%message
             {|"ppx_quick_test" was unable to convert a sexp example into the required type: |}
               ~sexp_example:(sexp : Sexp.t)
               ~raised_exception:(e : exn)])
  in
  match get_value_or_error ~of_sexp sexp_string with
  | Ok value -> value
  | Error error -> Error.raise error
;;

let get_parsed_examples t =
  match t with
  | NotProvided | Disabled -> []
  | Provided { sexp_strings; of_sexp; _ } ->
    List.map sexp_strings ~f:(value_of_string ~of_sexp)
;;

let insertion_for_new_example t (new_example : Sexp.t) =
  let new_example_str = new_example |> Sexp.to_string_hum |> string_to_ocaml_string in
  match t with
  | Disabled | NotProvided -> None
  | Provided { expression_placement_cnum; _ } ->
    Some
      { File_corrections.Insertion.placement_cnum = expression_placement_cnum
      ; value = [%string " %{new_example_str}"]
      }
;;
