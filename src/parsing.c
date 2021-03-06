// parsing

typedef struct {
  Arena * arena;
  Token token;
  char const * filename;
  char const * source;
} Parser;

typedef enum : u8 {
  PARSE_OK,
  PARSE_ERROR,
} ParseResult;

static Parser make_parser(Arena * arena, char const * filename, char const * source) {
  Parser t;

  t.arena = arena;
  t.token = next_token(source);
  t.filename = filename;
  t.source = source;

  return t;
}

static void report_parse_error(Parser * t, char const * message) {
  i64 line_number = 1;
  char const * position = t->token.start;
  char const * line_start = t->source;
  char const * p = t->source;

  while (p != position) {
    if (* p == '\n') {
      line_number = line_number + 1;
      line_start = p + 1;
    }

    p ++;
  }

  char const * q = line_start;

  while (* q != '\n' && * q != '\0') {
    q ++;
  }
  
  i64 line_length = q - line_start;
  i64 col = position - line_start;
  
  fprintf(
      stderr,
      "ERROR %s:%d:%d\n%.*s\n%*s^\n%*s%s: %s\n",
      t->filename,
      (int) line_number,
      (int) col,
      (int) line_length,
      line_start,
      (int) col, "",
      (int) col, "",
      token_name(t->token.tag),
      message
    );
}

static void parse_advance(Parser * t) {
  t->token = next_token(t->token.stop);
}

static ParseResult parse_consume(Parser * t, TokenTag tag) {
  if (t->token.tag != tag) {
    report_parse_error(t, "not expected token");
    return PARSE_ERROR;
  };

  parse_advance(t);
  return PARSE_OK;
}

typedef enum : u8 {
  BINDING_POWER_NONE,
  BINDING_POWER_ASSIGNMENT,
  BINDING_POWER_OR,         // right
  BINDING_POWER_AND,        // right
  BINDING_POWER_EQUALITY,   // left
  BINDING_POWER_COMPARISON, // left
  BINDING_POWER_TERM,       // left
  BINDING_POWER_FACTOR,     // left
  BINDING_POWER_PREFIX,
  BINDING_POWER_CALL,
  BINDING_POWER_PRIMARY,
} BindingPower;

typedef ParseResult (* ParseExprNullRule) (Parser *, Expr *);

typedef ParseResult (* ParseExprLeftRule) (Parser *, Expr *, Expr);

static ParseResult parse_expr_with_precedence(Parser *, Expr *, BindingPower);

static ParseResult parse_expr(Parser * t, Expr * e) {
  return parse_expr_with_precedence(t, e, BINDING_POWER_NONE);
}

static ParseResult parse_expr_grouping(Parser * t, Expr * e) {
  parse_advance(t);
  if (parse_expr(t, e)) return PARSE_ERROR;
  if (parse_consume(t, TOKEN_RPAREN)) return PARSE_ERROR;
  return PARSE_OK;
}

static ParseResult parse_neg(Parser * t, Expr * e) {
  Expr a;
  parse_advance(t);
  if (parse_expr_with_precedence(t, &a, BINDING_POWER_PREFIX)) return PARSE_ERROR;
  * e = make_expr_prefix(t->arena, EXPR_OP_NEG, a);
  return PARSE_OK;
}

static ParseResult parse_add(Parser * t, Expr * e, Expr a) {
  Expr b;
  parse_advance(t);
  if (parse_expr_with_precedence(t, &b, BINDING_POWER_TERM)) return PARSE_ERROR;
  * e = make_expr_infix(t->arena, EXPR_OP_ADD, a, b);
  return PARSE_OK;
}

static ParseResult parse_sub(Parser * t, Expr * e, Expr a) {
  Expr b;
  parse_advance(t);
  if (parse_expr_with_precedence(t, &b, BINDING_POWER_TERM)) return PARSE_ERROR;
  * e = make_expr_infix(t->arena, EXPR_OP_SUB, a, b);
  return PARSE_OK;
}

static ParseResult parse_mul(Parser * t, Expr * e, Expr a) {
  Expr b;
  parse_advance(t);
  if (parse_expr_with_precedence(t, &b, BINDING_POWER_FACTOR)) return PARSE_ERROR;
  * e = make_expr_infix(t->arena, EXPR_OP_MUL, a, b);
  return PARSE_OK;
}

static ParseResult parse_div(Parser * t, Expr * e, Expr a) {
  Expr b;
  parse_advance(t);
  if (parse_expr_with_precedence(t, &b, BINDING_POWER_FACTOR)) return PARSE_ERROR;
  * e = make_expr_infix(t->arena, EXPR_OP_DIV, a, b);
  return PARSE_OK;
}

static ParseResult parse_num(Parser * t, Expr * e) {
  * e = make_expr_literal(token_symbol(t->token));
  parse_advance(t);
  return PARSE_OK;
}

static ParseResult parse_null_rule_not_an_expr(Parser * t, __unused Expr * e) {
  report_parse_error(t, "expected expression");
  return PARSE_ERROR;
}

static ParseExprNullRule parse_null_rule(TokenTag tag) {
  switch (tag) {
    case TOKEN_LPAREN:
      return parse_expr_grouping;
    case TOKEN_NEG:
      return parse_neg;
    case TOKEN_NUM:
      return parse_num;
  }

  return parse_null_rule_not_an_expr;
}

static ParseExprLeftRule parse_left_rule(TokenTag tag) {
  switch (tag) {
    case TOKEN_ADD:
      return parse_add;
    case TOKEN_SUB:
      return parse_sub;
    case TOKEN_MUL:
      return parse_mul;
    case TOKEN_DIV:
      return parse_div;
  }

  return NULL;
}

static BindingPower parse_left_binding_power(TokenTag tag) {
  switch (tag) {
    case TOKEN_ADD:
      return BINDING_POWER_TERM;
    case TOKEN_SUB:
      return BINDING_POWER_TERM;
    case TOKEN_MUL:
      return BINDING_POWER_FACTOR;
    case TOKEN_DIV:
      return BINDING_POWER_FACTOR;
  }

  return BINDING_POWER_NONE;
}

static ParseResult parse_expr_with_precedence(Parser * t, Expr * e, BindingPower right_binding_power) {
  if (parse_null_rule(t->token.tag)(t, e)) return PARSE_ERROR;;

  while (right_binding_power < parse_left_binding_power(t->token.tag)) {
    if (parse_left_rule(t->token.tag)(t, e, * e)) return PARSE_ERROR;
  }

  return PARSE_OK;
}
