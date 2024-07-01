open Lwt.Infix

let () = Prometheus_unix.Logging.init ()
let program_name = "example"

let find_git_root dir =
  let cmd = [| "git"; "-C"; dir; "rev-parse"; "--show-toplevel" |] in
  Lwt_process.pread ("", cmd) >|= String.trim

let main config (*app*) mode repo =
  Lwt_main.run
    ( find_git_root repo >>= fun repo ->
      (* let webhook_secret = Current_github.App.webhook_secret app in *)
      let repo = Current_git.Local.v (Fpath.v repo) in
      let engine = Current.Engine.create ~config (Pipeline.v ~repo) in
      let routes =
        (* Routes.( *)
        (*   (s "webhooks" / s "github" /? nil) *)
        (*   @--> Current_github.webhook ~engine ~get_job_ids:Index.get_job_ids *)
        (*          ~webhook_secret) ::  *)
        Current_web.routes engine
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
        (const main
       $ Current.Config.cmdliner (* $ Current_github.App.cmdliner *)
       $ Current_web.cmdliner $ repo))

let () = exit @@ Cmd.eval cmd
