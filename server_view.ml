open Async
open Server

(* [st] is the initialized view_state of the server *)
let st = ref (init_state ())

(* [handle_connection] is the *)
let handle_connection _addr r w =
  let () = print_string ("New client \n") in
  let rec loop r w =
    Reader.read_line r >>= function
    | `Eof -> (printf "Error reading server\n"; return ())
    | `Ok line -> (print_endline ("received: " ^ line);
                   st := parse !st line r w;
                   Writer.write_line w (!st.res_string);
                   loop r w)
  in loop r w
(*Pipe.transfer (Reader.pipe r) (Writer.pipe w)
  (fun x -> parse x)*)

let quit_regex = Str.regexp {|^#quit\(;;\)?$|}

let matches s r =
  Str.string_match r s 0

let handle_stdin input =
  if matches input quit_regex then let _ = exit 0 in ()
  else print_string "Invalid command\n"; ()

let rec read_cmdline () =
  let stdin : Reader.t = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= fun res -> ignore(
    begin
      match res with
      | `Ok str -> return (handle_stdin str)
      | `Eof ->  return ()
    end);
  ignore (read_cmdline());
  Deferred.never ()

let create_tcp ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w -> handle_connection _addr r w) in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let run ~port =
  ignore (read_cmdline ());
  ignore (create_tcp port);
  Deferred.never ()

let main () =
  print_string "Starting chat server... \n";
  print_string "Enter \"#quit\" to shutdown the server. \n";
  print_string  "> ";
  Command.async
    ~summary:"Start the chat server"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 9999 int)
        ~doc:" Port to listen on (default 9999)"
    )
    (fun port () -> run ~port)
  |> Command.run

let () = main ()
