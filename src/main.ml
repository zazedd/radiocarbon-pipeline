(* This is the main entry-point for the executable.
   Edit [cmd] to set the text for "--help" and modify the command-line interface. *)

let () = Prometheus_unix.Logging.init ()

let program_name = "example"

let main repo =
  let repo = Current_git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create (Pipeline.v ~repo) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;  (* The main thread evaluating the pipeline. *)
      Current_web.run site;    (* Optional: provides a web UI *)
    ]
  end

let () = main "." |> ignore
