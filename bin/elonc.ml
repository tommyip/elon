open Arg
open Elon

let log_passes = ref []
let set_log_passes s =
  log_passes := String.split_on_char ',' s |> List.map @@ function
    | "lexer" | "preparser" | "parser" | "tychk" as p -> p
    | pass -> raise (Bad ("Unknown compiler pass `" ^ pass ^ "`"))

let path = ref None
let set_path p =
  if Option.is_some (!path) then
    raise (Bad "You can only pass one path argument");
  path := Some p

let arg_spec =
  [("--log", String set_log_passes, "Print debug log for specified passes (lexer, preparser, parser, tychk)")]

let reporter () =
  let open Logs in
  let report src level ~over k msgf =
    let name = Logs.Src.name src in
    if List.exists (String.equal name) !log_passes then
      let k _ = over (); k () in
      msgf @@ fun ?header ?tags fmt ->
        let _ = tags in (* Silence lints *)
        let header = CCOpt.or_ ~else_:(Some (Logs.Src.name src)) header in
        let ppf = if level = App then Format.std_formatter else Format.err_formatter in
        Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs_fmt.pp_header (level, header)
    else (over (); k ())
  in
  { report }

let init_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ())

let () =
  let usage_msg = "Usage: elonc [--log <lex>] <path>" in
  if Array.length Sys.argv = 1 then
    Arg.usage arg_spec usage_msg
  else begin
    Arg.parse arg_spec set_path usage_msg;
    match !path with
    | None -> raise (Bad "Missing path argument")
    | Some path ->
        if not (Sys.file_exists path) then
          raise (Bad "File path does not exist");
        init_logs ();
        let ctx = Driver.({ path }) in
        Driver.compile ctx
  end
