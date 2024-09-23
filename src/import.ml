module Github = Current_github
module Git = Current_git

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.( >>= )
let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

type status =
  [ `No_changes
  | `Internal_error of string list
  | `Csv_changes of string list
  | `Config_changes of string list
  | `Script_changes of string list
  | `Multiple_changes of string list ]

let status_file_list_to_strings lst =
  "Files affected: "
  ^ (lst |> List.map (Format.sprintf "%S") |> String.concat ", ")

let status_to_string = function
  | `No_changes -> "No changes"
  | `Internal_error lst -> "Internal Error: " ^ status_file_list_to_strings lst
  | `Csv_changes lst -> "CSV changes. " ^ status_file_list_to_strings lst
  | `Config_changes lst -> "Config changes. " ^ status_file_list_to_strings lst
  | `Script_changes lst -> "Script changes. " ^ status_file_list_to_strings lst
  | `Multiple_changes lst ->
      "Multiple changes. " ^ status_file_list_to_strings lst

let status_to_gh_status ?msg :
    [ `Success | `Failed ] -> Github.Api.CheckRunStatus.conclusion = function
  | `Success -> `Success
  | `Failed -> `Failure (Option.get msg)

let set_commit_status commit description status check_name =
  let status =
    Github.Api.CheckRunStatus.v ?text:(Some description) status
    |> Current.return
  in
  Github.Api.CheckRun.set_status commit check_name status

let rec get_suffix path cmp acc =
  if path = Fpath.v "/" then Fpath.base acc
  else if path = cmp then acc
  else
    let new_path, b = Fpath.split_base path in
    if cmp = b then acc
    else get_suffix (new_path |> Fpath.rem_empty_seg) cmp Fpath.(b // acc)

let file_path cmp t =
  let input_dir, name = Fpath.split_base t in
  let input_dir = input_dir |> Fpath.rem_empty_seg in
  get_suffix input_dir cmp name |> Fpath.to_string

let rec base_dir path =
  let a = Fpath.split_base path |> fst in
  if a = Fpath.v "./" then path else base_dir a
