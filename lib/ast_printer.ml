open! Prelude

let rec print_expr : Ast_expr.t -> unit = function
  | Call (f, args) ->
    printf "(";
    print_expr f;
    Array.iter args ~f:(fun arg -> printf " "; print_expr arg);
    printf ")"
  | If (p, a, b) ->
    printf "(if (";
    print_expr p;
    print_block a;
    printf ") (else";
    print_block b;
    printf "))"
  | Int n ->
    printf "%d" n
  | Var name ->
    printf "%s" name
  | Op1 (o, a) ->
    printf "(%s " (Ast_op1.to_string o);
    print_expr a;
    printf ")"
  | Op2 (o, a, b) ->
    printf "(%s " (Ast_op2.to_string o);
    print_expr a;
    printf " ";
    print_expr b;
    printf ")"
  | Loop b ->
    printf "(loop";
    print_block b;
    printf ")"

and print_stmt : Ast_stmt.t -> unit = function
  | Expr e ->
    print_expr e
  | Let1 (s, e) ->
    printf "(let %s = " s;
    print_expr e;
    printf ")";

and print_block (block : Ast_block.t) =
  Array.iter block.seq ~f:(fun stmt -> printf " "; print_stmt stmt);
  match block.jump with
  | Break ->
    printf " break"
  | None ->
    ()
  | Return0 ->
    printf " return"
  | Return1 e ->
    printf " (return ";
    print_expr e;
    printf ")"
