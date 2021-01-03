open Elon

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level ~all:true (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());

  let path = ref None in
  let set_path p =
    if Option.is_some (!path) then
      failwith "You can only pass one path argument";
    path := Some p
  in
  let usage_msg = "Usage: elonc <path>" in
  Arg.parse [] set_path usage_msg;
  match !path with
  | None -> failwith "Missing path argument"
  | Some path ->
      if not (Sys.file_exists path) then
        failwith "File path does not exist"
      else
        let ctx = Driver.({ path }) in
        Driver.compile ctx
