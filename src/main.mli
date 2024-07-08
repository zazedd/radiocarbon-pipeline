val main :
  Current.Config.t ->
  Import.Github.App.t ->
  Conduit_lwt_unix.server ->
  string ->
  ('a, [ `Msg of string ]) result
