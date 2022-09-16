open! Prelude

let go () =
  Out_channel.flush Out_channel.stdout;
  let source = In_channel.input_all In_channel.stdin in
  let p = Parser.make source in
  let e = Parser.parse_expr p in
  Ast_printer.print_expr e;
  printf "\n";
  Out_channel.flush Out_channel.stdout
