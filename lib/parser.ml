open! Prelude

type t = Parser_cursor.t =
  { source : string
  ; mutable token : Token.t
  ; mutable token_start : int
  ; mutable token_stop : int
  }

exception Parse_error

let make source =
  let t =
    { source
    ; token = Error
    ; token_start = 0
    ; token_stop = 0
    }
  in
  Lexer.next t;
  t

let token t =
  t.token

let token_string t =
  String.sub t.source t.token_start (t.token_stop - t.token_start)

let token_symbol t =
  Ast_symbol.make t.source ~pos:t.token_start ~len:(t.token_stop - t.token_start)

let advance t =
  Lexer.next t

let expect t a =
  if not (Token.equal a (token t)) then raise Parse_error

let consume t a =
  expect t a;
  advance t

let rec parse_block = 
  let finish a b =
    { Ast_block.
      seq = Array.of_list_rev a
    ; jump = b
    }
  in
  let rec loop t a =
    match token t with
    | End | Elif | Else ->
      finish a None
    | Break ->
      advance t;
      finish a Break
    | Return ->
      advance t;
      begin match token t with
      | End | Elif | Else ->
        finish a Return0
      | _ ->
        let e = parse_expr t in
        finish a (Return1 e)
      end
    | Let ->
      advance t;
      expect t Symbol;
      let s = token_symbol t in
      advance t;
      consume t Assignment;
      let e = parse_expr t in
      loop t (Ast_stmt.Let1 (s, e) :: a)
    | _ ->
      let e = parse_expr t in
      loop t (Ast_stmt.Expr e :: a)
  in
  fun t ->
    loop t []

and parse_expr t =
  parse_expr_equality t

and parse_expr_equality =
  let rec f t e =
    match token t with
    | Equal -> g t e Ast_op2.Equal
    | Not_equal -> g t e Ast_op2.Not_equal
    | _ -> e
  and g t e o =
    advance t;
    let r = parse_expr_comparison t in
    f t (Ast_expr.Op2 (o, e, r))
  in
  fun t ->
    f t (parse_expr_comparison t)

and parse_expr_comparison =
  let rec f t e =
    match token t with
    | Greater_than -> g t e Ast_op2.Greater_than
    | Greater_than_or_equal -> g t e Ast_op2.Greater_than_or_equal
    | Less_than -> g t e Ast_op2.Less_than
    | Less_than_or_equal -> g t e Ast_op2.Less_than_or_equal
    | _ -> e
  and g t e o =
    advance t;
    let r = parse_expr_term t in
    f t (Ast_expr.Op2 (o, e, r))
  in
  fun t ->
    f t (parse_expr_term t)

and parse_expr_term =
  let rec f t e =
    match token t with
    | Plus -> g t e Ast_op2.Plus
    | Minus -> g t e Ast_op2.Minus
    | _ -> e
  and g t e o =
    advance t;
    let r = parse_expr_factor t in
    f t (Ast_expr.Op2 (o, e, r))
  in
  fun t ->
    f t (parse_expr_factor t)

and parse_expr_factor =
  let rec f t e =
    match token t with
    | Star -> g t e Ast_op2.Star
    | Slash -> g t e Ast_op2.Slash
    | _ -> e
  and g t e o =
    advance t;
    let r = parse_expr_unary t in
    f t (Ast_expr.Op2 (o, e, r))
  in
  fun t ->
    f t (parse_expr_unary t)

and parse_expr_unary = 
  let f t o =
    advance t;
    let r = parse_expr_unary t in
    Ast_expr.Op1 (o, r)
  in
  fun t ->
    match token t with
    | Tilde -> f t Ast_op1.Tilde
    | _ -> parse_expr_call t

and parse_expr_call =
  let rec f t e =
    match token t with
    | L_paren_no_space ->
      advance t;
      begin match token t with
      | R_paren ->
        advance t;
        f t (Ast_expr.Call (e, [||]))
      | _ ->
        g t e []
      end
    | _ ->
      e
  and g t e a =
    let b = parse_expr t in
    let a = b :: a in
    match token t with
    | Comma ->
      advance t;
      g t e a
    | R_paren ->
      advance t;
      f t (Ast_expr.Call (e, Array.of_list_rev a))
    | _ ->
      raise Parse_error
  in
  fun t ->
    let e = parse_expr_primary t in
    f t e

and parse_expr_primary t =
  match token t with
  | Number ->
    let s = token_string t in
    let n = try int_of_string s with _ -> raise Parse_error in
    advance t;
    Ast_expr.Int n
  | Symbol ->
    let s = token_symbol t in
    advance t;
    Ast_expr.Var s
  | L_paren | L_paren_no_space ->
    advance t;
    let e = parse_expr t in
    consume t R_paren;
    e
  | Loop ->
    advance t;
    let a = parse_block t in
    Ast_expr.Loop a
  | If ->
    advance t;
    let p = parse_expr t in
    let a = parse_block t in
    begin match token t with
    | End ->
      advance t;
      Ast_expr.If (p, a, Ast_block.empty)
    | Else ->
      advance t;
      let b = parse_block t in
      consume t End;
      Ast_expr.If (p, a, b)
    | Elif ->
      (* TODO: elif *)
      raise Parse_error
    | _ ->
      raise Parse_error
    end
  | _ -> 
    raise Parse_error

let parse_decl t =
  match token t with
  | Fun ->
    advance t;
    expect t Symbol;
    let name = token_symbol t in
    advance t;
    consume t L_paren_no_space;
    consume t R_paren;
    let body = parse_block t in
    consume t End;
    Ast_decl.Fun { name; parameters = [||]; body }
  | _ ->
    raise Parse_error
