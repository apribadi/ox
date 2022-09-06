open! Prelude

let go () =
  let source = In_channel.input_all In_channel.stdin in
  let p = Parser.make source in
  let s = Parser.parse_stmt p in
  Ast_printer.print_stmt s;
  printf "\n";
  Out_channel.flush Out_channel.stdout
