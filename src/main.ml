open! Import
open Lwt.Infix

let () = Prometheus_unix.Logging.init ()
let program_name = "example"

let find_git_root dir =
  let cmd = [| "git"; "-C"; dir; "rev-parse"; "--show-toplevel" |] in
  let rec h s =
    if s = "" then Lwt_process.pread ("", cmd) >|= String.trim >>= fun l -> h l
    else s |> Lwt.return
  in
  h ""

let main config app mode account id repo =
  Lwt_main.run
    ( find_git_root repo >>= fun local ->
      let get_job_ids ~owner:_ ~name:_ ~hash:_ = [] in
      let local = Current_git.Local.v (Fpath.v local) in
      let webhook_secret = Current_github.App.webhook_secret app in
      let installation =
        Github.App.installation app ~account id |> Current.return
      in
      let engine =
        Current.Engine.create ~config (Pipeline.v ~local ~installation)
      in
      let routes =
        Routes.(
          (s "webhooks" / s "github" /? nil)
          @--> Current_github.webhook ~engine ~get_job_ids ~webhook_secret)
        :: Current_web.routes engine
      in
      let site =
        Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes
      in
      Lwt.choose
        [
          Current.Engine.thread engine;
          (* The main thread evaluating the pipeline. *)
          Current_web.run ~mode site;
          (* Optional: provides a web UI *)
        ] )

open Cmdliner

let account =
  Arg.value
  @@ Arg.opt Arg.(string) ""
  @@ Arg.info ~doc:"The account that has the Github App installation."
       [ "github-app-account" ]

let app_id =
  Arg.value
  @@ Arg.opt Arg.(int) 0
  @@ Arg.info
       ~doc:"The Github Installation id associated with the github account."
       [ "github-inst-id" ]

let repo =
  Arg.value
  @@ Arg.pos 0 Arg.dir (Sys.getcwd ())
  @@ Arg.info ~doc:"The directory containing the .git subdirectory." ~docv:"DIR"
       []

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info
    Term.(
      term_result
        (const main $ Current.Config.cmdliner $ Current_github.App.cmdliner
       $ Current_web.cmdliner $ account $ app_id $ repo))

let () = exit @@ Cmd.eval cmd
