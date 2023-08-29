open! Core

module Insertion = struct
  type t =
    { placement_cnum : int
    ; value : string
    }
  [@@deriving sexp]

  let apply_all ~original insertions =
    let verify_insertions_range () =
      let all_insertions_in_range =
        List.for_all insertions ~f:(fun { placement_cnum; _ } ->
          0 <= placement_cnum && placement_cnum <= String.length original)
      in
      match all_insertions_in_range with
      | true -> ()
      | false ->
        raise_s
          [%message
            {|"ppx_quick_test" bug! Attempted to insert correction at at out of bounds location.|}
              (insertions : t list)
              ~original_legnth:(String.length original : int)
              (original : string)]
    in
    let get_and_verify_insertion_map insertions =
      let insertion_map =
        List.map insertions ~f:(fun { placement_cnum; value } -> placement_cnum, value)
        |> Map.of_alist_or_error (module Int)
      in
      match insertion_map with
      | Ok map -> map
      | Error error ->
        raise_s
          [%message
            {|"ppx_quick_test" bug! Attempt to insert multiple corrections at the same location.|}
              (error : Error.t)
              (insertions : t list)
              (original : string)]
    in
    verify_insertions_range ();
    let insertions_map = get_and_verify_insertion_map insertions in
    let total_output_length =
      String.length original
      + List.sum (module Int) insertions ~f:(fun t -> String.length t.value)
    in
    let buffer = Buffer.create total_output_length in
    let insert_if_found ~index =
      match Map.find insertions_map index with
      | None -> ()
      | Some str_to_insert -> Buffer.add_string buffer str_to_insert
    in
    String.iteri original ~f:(fun i c ->
      insert_if_found ~index:i;
      Buffer.add_char buffer c);
    insert_if_found ~index:(String.length original);
    Buffer.contents buffer
  ;;
end

type t =
  { mutable insertions : Insertion.t list
  ; filename : string
  ; mutable disabled : bool
  }

let global_registry = ref String.Map.empty

let create ~filename =
  let new_ = { insertions = []; filename; disabled = false } in
  global_registry
  := match Map.add !global_registry ~key:filename ~data:new_ with
    | `Ok map -> map
    | `Duplicate ->
      raise_s
        [%message
          "ppx_quick_test bug: tried to create a registry for the same file twice"
            ~duplicate:(filename : string)]
;;

let make_corrected_file ~filename =
  match Map.find !global_registry filename with
  | None ->
    raise_s
      [%message
        "ppx_quick_test bug: tried to create a corrected file for a file that was not \
         registered"
          ~unregistered_file:(filename : string)]
  | Some t ->
    (match `Disabled t.disabled, t.insertions with
     | `Disabled true, _ | _, [] -> ()
     | _, insertions ->
       let filename = Filename.basename t.filename in
       let prev_contents = In_channel.read_all filename in
       let next_contents = Insertion.apply_all ~original:prev_contents insertions in
       (match Make_corrected_file.f ~path:filename ~next_contents () with
        | Ok _ -> ()
        | Error _ ->
          let message =
            {|"ppx_quick_test" found failing tests and suggests adding the inputs to your examples for regression testing.|}
          in
          Stdio.eprintf "%s\n%!" message;
          exit 1))
;;

let add_insertion ~filename insertion =
  match Map.find !global_registry filename with
  | None ->
    raise_s
      [%message
        "ppx_quick_test bug: tried to add an insertion for a file that was not registered"
          ~unregistered_file:(filename : string)]
  | Some t -> t.insertions <- insertion :: t.insertions
;;

let disable_due_to_pending_error ~filename =
  match Map.find !global_registry filename with
  | None ->
    raise_s
      [%message
        "ppx_quick_test bug: tried to disable corrections for a filename that was never \
         registered"
          ~unregistered_file:(filename : string)]
  | Some t -> t.disabled <- true
;;
