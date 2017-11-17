open Core
open Async
open Unix

let chat addr r w =
  printf "> ";
  let stdin = Lazy.force Reader.stdin in
  let stdout = Lazy.force Writer.stdout in
  (* We transfer stdin over our TCP connection. *)
  don't_wait_for (Reader.transfer stdin (Writer.pipe w));
  (* And, we simultaneously transfer the inbound TPC traffic to to stdout. *)
  don't_wait_for (Reader.transfer r (Writer.pipe stdout));
  Deferred.never ()

let run ~host ~port =
  let addr = Tcp.to_host_and_port host port in
  ignore(Tcp.with_connection addr chat);
  Deferred.never ()

let main () =
  print_string "Starting Caml Chat... \n";
  print_string "Enter \"#quit\" to exit. \n";
  Command.async
    ~summary:"Start the chat client"
    Command.Spec.(
      empty
      +> flag "-host" (optional_with_default "127.0.0.1" string)
        ~doc:" Port to listen on (default 9999)"
      +> flag "-port" (optional_with_default 9999 int)
        ~doc:" Port to listen on (default 9999)"
    )
    (fun host port () -> run ~host ~port)
  |> Command.run

let () = main ()
