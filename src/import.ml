module Github = Current_github
module Git = Current_git

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.( >>= )
let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

let set_commit_status commit description status check_name =
  let status =
    Github.Api.CheckRunStatus.v ?text:(Some description) status
    |> Current.return
  in
  Github.Api.CheckRun.set_status commit check_name status
