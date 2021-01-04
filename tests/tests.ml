open Alcotest

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());

  run "Elon" [
    Preparser.test_suite;
  ]
