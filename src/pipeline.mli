val timeout : Duration.t

(* val vv : *)
(*   src:Current_git.Commit.t Current.t -> *)
(*   local_src:Current_git.Commit.t Current.t -> *)
(*   github_commit:Current_github.Api.Commit.t Current.t -> *)
(*   unit -> *)
(*   [> `Deleted_inputs | `New_inputs | `No_inputs ] Current.t *)

val v :
  local:Import.Git.Local.t ->
  installation:Import.Github.Installation.t Current.t ->
  unit ->
  unit Current.t
