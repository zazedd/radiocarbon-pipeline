module Git = Current_git

let v ~repo () =
  (* Replace this with your actual pipeline: *)
  let src = Git.Local.head_commit repo in
  (*Format.printf "hi@.";*)
  Current.ignore_value src
