// lexing
//
// Token next_token(char const * position)

typedef enum : u8 {
  // punctuation

  TOKEN_ASSIGN,
  TOKEN_COMMA,
  TOKEN_DOT,
  TOKEN_COLON,
  TOKEN_SEMICOLON,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_LBRACKET,
  TOKEN_RBRACKET,
  TOKEN_LPAREN,
  TOKEN_RPAREN,

  // op

  TOKEN_ADD,
  TOKEN_SUB,
  TOKEN_MUL,
  TOKEN_DIV,
  TOKEN_NEG,
  TOKEN_EQ,
  TOKEN_NE,
  TOKEN_GE,
  TOKEN_GT,
  TOKEN_LE,
  TOKEN_LT,

  // keyword

  TOKEN_AND,
  TOKEN_CASE,
  TOKEN_DO,
  TOKEN_ELIF,
  TOKEN_ELSE,
  TOKEN_END,
  TOKEN_FOR,
  TOKEN_FUNCTION,
  TOKEN_IF,
  TOKEN_LET,
  TOKEN_OR,
  TOKEN_THEN,
  TOKEN_WHILE,

  // misc

  TOKEN_ID,
  TOKEN_NUM,
  TOKEN_EOF,
  TOKEN_ILLEGAL_CHAR,
} TokenTag;

static char const * token_name(TokenTag t) {
  switch (t) {
    case TOKEN_ASSIGN: return "ASSIGN";
    case TOKEN_COMMA: return "COMMA";
    case TOKEN_DOT: return "DOT";
    case TOKEN_COLON: return "COLON";
    case TOKEN_SEMICOLON: return "SEMICOLON";
    case TOKEN_LBRACE: return "LBRACE";
    case TOKEN_RBRACE: return "RBRACE";
    case TOKEN_LBRACKET: return "LBRACKET";
    case TOKEN_RBRACKET: return "RBRACKET";
    case TOKEN_LPAREN: return "LPAREN";
    case TOKEN_RPAREN: return "RPAREN";
    case TOKEN_ADD: return "ADD";
    case TOKEN_SUB: return "SUB";
    case TOKEN_MUL: return "MUL";
    case TOKEN_DIV: return "DIV";
    case TOKEN_NEG: return "NEG";
    case TOKEN_EQ: return "EQ";
    case TOKEN_NE: return "NE";
    case TOKEN_GE: return "GE";
    case TOKEN_GT: return "GT";
    case TOKEN_LE: return "LE";
    case TOKEN_LT: return "LT";
    case TOKEN_AND: return "AND";
    case TOKEN_CASE: return "CASE";
    case TOKEN_DO: return "DO";
    case TOKEN_ELIF: return "ELIF";
    case TOKEN_ELSE: return "ELSE";
    case TOKEN_END: return "END";
    case TOKEN_FOR: return "FOR";
    case TOKEN_FUNCTION: return "FUNCTION";
    case TOKEN_IF: return "IF";
    case TOKEN_LET: return "LET";
    case TOKEN_OR: return "OR";
    case TOKEN_THEN: return "THEN";
    case TOKEN_WHILE: return "WHILE";
    case TOKEN_ID: return "ID";
    case TOKEN_NUM: return "NUM";
    case TOKEN_EOF: return "EOF";
    case TOKEN_ILLEGAL_CHAR: return "ILLEGAL_CHAR";
  }
}

// token

typedef struct {
  TokenTag tag;
  char const * start;
  char const * stop;
} Token;

static inline Token make_token(TokenTag tag, char const * start, char const * stop) {
  return (Token) { .tag = tag, .start = start, .stop = stop };
}

static Symbol token_symbol(Token t) {
  return make_symbol(t.start, t.stop);
}

typedef enum : u8 {
  CHAR_CLASS_UNUSED = 0,
  CHAR_CLASS_ALPHA,
  CHAR_CLASS_DIGIT,
  CHAR_CLASS_HASH,
  CHAR_CLASS_MINUS,
  CHAR_CLASS_NEWLINE,
  CHAR_CLASS_NULL,
  CHAR_CLASS_OP,
  CHAR_CLASS_PUNCT,
  CHAR_CLASS_SPACE,
} CharClass;

#define CHAR_CLASS_COUNT 10

typedef enum : u8 {
  LEXER_STATE_ID,
  LEXER_STATE_MINUS,
  LEXER_STATE_NUM,
  LEXER_STATE_OP,
  LEXER_STATE_COMMENT,
  LEXER_STATE_START,
  LEXER_STATE_COMPLETE_ID,
  LEXER_STATE_COMPLETE_NUM,
  LEXER_STATE_COMPLETE_OP,
  LEXER_STATE_EOF,
  LEXER_STATE_ILLEGAL_CHAR,
  LEXER_STATE_PUNCT,
} LexerState;

#define LEXER_STATE_COUNT_MULTICHAR_TOKEN 4
#define LEXER_STATE_COUNT_NONTERMINAL 6
#define LEXER_STATE_COUNT 12

static CharClass const char_class_table[256];

static LexerState const lexer_transition_table[LEXER_STATE_COUNT_NONTERMINAL][CHAR_CLASS_COUNT];

static Token (* const lexer_jump_table[LEXER_STATE_COUNT])(LexerState, char const *, i64);

static Token next_token__loop(LexerState s, char const * p, i64 n) {
  n = n + (s < LEXER_STATE_COUNT_MULTICHAR_TOKEN);
  s = lexer_transition_table[s][char_class_table[(u8) * p]];
  p = p + 1;

  return lexer_jump_table[s](s, p, n);
}

static inline Token next_token(char const * position) {
  return next_token__loop(LEXER_STATE_START, position, 0);
}

static Token next_token__eof(__unused LexerState s, char const * p, __unused i64 n) {
  char const * start = p - 1;
  char const * stop = p - 1;

  return make_token(TOKEN_EOF, start, stop);
}

static Token next_token__illegal_char(__unused LexerState s, char const * p, __unused i64 n) {
  char const * start = p - 1;
  char const * stop = p;

  return make_token(TOKEN_ILLEGAL_CHAR, start, stop);
}

static Token next_token__id(__unused LexerState s, char const * p, i64 n) {
  char const * start = p - 1 - n;
  char const * stop = p - 1;

  switch (n) {
    case 2:
      if (!bcmp(start, "do", 2)) return make_token(TOKEN_DO, start, stop);
      if (!bcmp(start, "if", 2)) return make_token(TOKEN_IF, start, stop);
      if (!bcmp(start, "or", 2)) return make_token(TOKEN_OR, start, stop);
      break;
    case 3:
      if (!bcmp(start, "and", 3)) return make_token(TOKEN_AND, start, stop);
      if (!bcmp(start, "end", 3)) return make_token(TOKEN_END, start, stop);
      if (!bcmp(start, "for", 3)) return make_token(TOKEN_FOR, start, stop);
      if (!bcmp(start, "let", 3)) return make_token(TOKEN_LET, start, stop);
      break;
    case 4:
      if (!bcmp(start, "case", 4)) return make_token(TOKEN_CASE, start, stop);
      if (!bcmp(start, "elif", 4)) return make_token(TOKEN_ELIF, start, stop);
      if (!bcmp(start, "else", 4)) return make_token(TOKEN_ELSE, start, stop);
      if (!bcmp(start, "then", 4)) return make_token(TOKEN_THEN, start, stop);
      break;
    case 5:
      if (!bcmp(start, "while", 5)) return make_token(TOKEN_WHILE, start, stop);
      break;
    case 8:
      if (!bcmp(start, "function", 8)) return make_token(TOKEN_FUNCTION, start, stop);
      break;
  }

  return make_token(TOKEN_ID, start, stop);
}

static Token next_token__num(__unused LexerState s, char const * p, i64 n) {
  char const * start = p - 1 - n;
  char const * stop = p - 1;

  return make_token(TOKEN_NUM, start, stop);
}

static Token next_token__op(__unused LexerState s, char const * p, i64 n) {
  char const * start = p - 1 - n;
  char const * stop = p - 1;

  switch (n) {
    case 1:
      switch (* start) {
        case '=': return make_token(TOKEN_ASSIGN, start, stop);
        case '+': return make_token(TOKEN_ADD, start, stop);
        case '-': return make_token(TOKEN_SUB, start, stop);
        case '*': return make_token(TOKEN_MUL, start, stop);
        case '~': return make_token(TOKEN_NEG, start, stop);
        case '/': return make_token(TOKEN_DIV, start, stop);
        case '<': return make_token(TOKEN_LT, start, stop);
        case '>': return make_token(TOKEN_GT, start, stop);
      }
      break;
    case 2:
      if (* (start + 1) == '=') {
        switch (* start) {
          case '=': return make_token(TOKEN_EQ, start, stop);
          case '!': return make_token(TOKEN_NE, start, stop);
          case '<': return make_token(TOKEN_LE, start, stop);
          case '>': return make_token(TOKEN_GE, start, stop);
        }
      }
      break;
  };

  return make_token(TOKEN_ILLEGAL_CHAR, start, stop);
}

static Token next_token__punct(__unused LexerState s, char const * p, __unused i64 n) {
  char const * start = p - 1;
  char const * stop = p;

  switch (* start) {
    case ',': return make_token(TOKEN_COMMA, start, stop);
    case '.': return make_token(TOKEN_DOT, start, stop);
    case ':': return make_token(TOKEN_COLON, start, stop);
    case ';': return make_token(TOKEN_SEMICOLON, start, stop);
    case '{': return make_token(TOKEN_LBRACE, start, stop);
    case '}': return make_token(TOKEN_RBRACE, start, stop);
    case '[': return make_token(TOKEN_LBRACKET, start, stop);
    case ']': return make_token(TOKEN_RBRACKET, start, stop);
    case '(': return make_token(TOKEN_LPAREN, start, stop);
    case ')': return make_token(TOKEN_RPAREN, start, stop);
  };

  // impossible

  return make_token(TOKEN_ILLEGAL_CHAR, start, stop);
}

static CharClass const char_class_table[256] = {
  ['\0'] = CHAR_CLASS_NULL,
  ['\t'] = CHAR_CLASS_SPACE,
  ['\n'] = CHAR_CLASS_NEWLINE,
  [' '] = CHAR_CLASS_SPACE,
  ['!'] = CHAR_CLASS_OP,
  ['#'] = CHAR_CLASS_HASH,
  ['$'] = CHAR_CLASS_OP,
  ['%'] = CHAR_CLASS_OP,
  ['&'] = CHAR_CLASS_OP,
  ['('] = CHAR_CLASS_PUNCT,
  [')'] = CHAR_CLASS_PUNCT,
  ['*'] = CHAR_CLASS_OP,
  ['+'] = CHAR_CLASS_OP,
  [','] = CHAR_CLASS_PUNCT,
  ['-'] = CHAR_CLASS_MINUS,
  ['.'] = CHAR_CLASS_PUNCT,
  ['/'] = CHAR_CLASS_OP,
  ['0'] = CHAR_CLASS_DIGIT,
  ['1'] = CHAR_CLASS_DIGIT,
  ['2'] = CHAR_CLASS_DIGIT,
  ['3'] = CHAR_CLASS_DIGIT,
  ['4'] = CHAR_CLASS_DIGIT,
  ['5'] = CHAR_CLASS_DIGIT,
  ['6'] = CHAR_CLASS_DIGIT,
  ['7'] = CHAR_CLASS_DIGIT,
  ['8'] = CHAR_CLASS_DIGIT,
  ['9'] = CHAR_CLASS_DIGIT,
  [':'] = CHAR_CLASS_PUNCT,
  [';'] = CHAR_CLASS_PUNCT,
  ['<'] = CHAR_CLASS_OP,
  ['='] = CHAR_CLASS_OP,
  ['>'] = CHAR_CLASS_OP,
  ['?'] = CHAR_CLASS_OP,
  ['@'] = CHAR_CLASS_OP,
  ['A'] = CHAR_CLASS_ALPHA,
  ['B'] = CHAR_CLASS_ALPHA,
  ['C'] = CHAR_CLASS_ALPHA,
  ['D'] = CHAR_CLASS_ALPHA,
  ['E'] = CHAR_CLASS_ALPHA,
  ['F'] = CHAR_CLASS_ALPHA,
  ['G'] = CHAR_CLASS_ALPHA,
  ['H'] = CHAR_CLASS_ALPHA,
  ['I'] = CHAR_CLASS_ALPHA,
  ['J'] = CHAR_CLASS_ALPHA,
  ['K'] = CHAR_CLASS_ALPHA,
  ['L'] = CHAR_CLASS_ALPHA,
  ['M'] = CHAR_CLASS_ALPHA,
  ['N'] = CHAR_CLASS_ALPHA,
  ['O'] = CHAR_CLASS_ALPHA,
  ['P'] = CHAR_CLASS_ALPHA,
  ['Q'] = CHAR_CLASS_ALPHA,
  ['R'] = CHAR_CLASS_ALPHA,
  ['S'] = CHAR_CLASS_ALPHA,
  ['T'] = CHAR_CLASS_ALPHA,
  ['U'] = CHAR_CLASS_ALPHA,
  ['V'] = CHAR_CLASS_ALPHA,
  ['W'] = CHAR_CLASS_ALPHA,
  ['X'] = CHAR_CLASS_ALPHA,
  ['Y'] = CHAR_CLASS_ALPHA,
  ['Z'] = CHAR_CLASS_ALPHA,
  ['['] = CHAR_CLASS_PUNCT,
  [']'] = CHAR_CLASS_PUNCT,
  ['^'] = CHAR_CLASS_OP,
  ['_'] = CHAR_CLASS_ALPHA,
  ['a'] = CHAR_CLASS_ALPHA,
  ['b'] = CHAR_CLASS_ALPHA,
  ['c'] = CHAR_CLASS_ALPHA,
  ['d'] = CHAR_CLASS_ALPHA,
  ['e'] = CHAR_CLASS_ALPHA,
  ['f'] = CHAR_CLASS_ALPHA,
  ['g'] = CHAR_CLASS_ALPHA,
  ['h'] = CHAR_CLASS_ALPHA,
  ['i'] = CHAR_CLASS_ALPHA,
  ['j'] = CHAR_CLASS_ALPHA,
  ['k'] = CHAR_CLASS_ALPHA,
  ['l'] = CHAR_CLASS_ALPHA,
  ['m'] = CHAR_CLASS_ALPHA,
  ['n'] = CHAR_CLASS_ALPHA,
  ['o'] = CHAR_CLASS_ALPHA,
  ['p'] = CHAR_CLASS_ALPHA,
  ['q'] = CHAR_CLASS_ALPHA,
  ['r'] = CHAR_CLASS_ALPHA,
  ['s'] = CHAR_CLASS_ALPHA,
  ['t'] = CHAR_CLASS_ALPHA,
  ['u'] = CHAR_CLASS_ALPHA,
  ['v'] = CHAR_CLASS_ALPHA,
  ['w'] = CHAR_CLASS_ALPHA,
  ['x'] = CHAR_CLASS_ALPHA,
  ['y'] = CHAR_CLASS_ALPHA,
  ['z'] = CHAR_CLASS_ALPHA,
  ['{'] = CHAR_CLASS_PUNCT,
  ['|'] = CHAR_CLASS_OP,
  ['}'] = CHAR_CLASS_PUNCT,
  ['~'] = CHAR_CLASS_OP,
};

static LexerState const lexer_transition_table[LEXER_STATE_COUNT_NONTERMINAL][CHAR_CLASS_COUNT] = {
  [LEXER_STATE_ID] = {
    [CHAR_CLASS_UNUSED] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_ALPHA] = LEXER_STATE_ID,
    [CHAR_CLASS_DIGIT] = LEXER_STATE_ID,
    [CHAR_CLASS_HASH] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_MINUS] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_NEWLINE] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_NULL] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_OP] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_PUNCT] = LEXER_STATE_COMPLETE_ID,
    [CHAR_CLASS_SPACE] = LEXER_STATE_COMPLETE_ID,
  },
  [LEXER_STATE_MINUS] = {
    [CHAR_CLASS_UNUSED] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_ALPHA] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_DIGIT] = LEXER_STATE_NUM,
    [CHAR_CLASS_HASH] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_MINUS] = LEXER_STATE_OP,
    [CHAR_CLASS_NEWLINE] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_NULL] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_OP] = LEXER_STATE_OP,
    [CHAR_CLASS_PUNCT] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_SPACE] = LEXER_STATE_COMPLETE_OP,
  },
  [LEXER_STATE_NUM] = {
    [CHAR_CLASS_UNUSED] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_ALPHA] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_DIGIT] = LEXER_STATE_NUM,
    [CHAR_CLASS_HASH] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_MINUS] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_NEWLINE] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_NULL] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_OP] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_PUNCT] = LEXER_STATE_COMPLETE_NUM,
    [CHAR_CLASS_SPACE] = LEXER_STATE_COMPLETE_NUM,
  },
  [LEXER_STATE_OP] = {
    [CHAR_CLASS_UNUSED] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_ALPHA] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_DIGIT] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_HASH] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_MINUS] = LEXER_STATE_OP,
    [CHAR_CLASS_NEWLINE] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_NULL] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_OP] = LEXER_STATE_OP,
    [CHAR_CLASS_PUNCT] = LEXER_STATE_COMPLETE_OP,
    [CHAR_CLASS_SPACE] = LEXER_STATE_COMPLETE_OP,
  },
  [LEXER_STATE_COMMENT] = {
    [CHAR_CLASS_UNUSED] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_ALPHA] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_DIGIT] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_HASH] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_MINUS] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_NEWLINE] = LEXER_STATE_START,
    [CHAR_CLASS_NULL] = LEXER_STATE_EOF,
    [CHAR_CLASS_OP] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_PUNCT] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_SPACE] = LEXER_STATE_COMMENT,
  },
  [LEXER_STATE_START] = {
    [CHAR_CLASS_UNUSED] = LEXER_STATE_ILLEGAL_CHAR,
    [CHAR_CLASS_ALPHA] = LEXER_STATE_ID,
    [CHAR_CLASS_DIGIT] = LEXER_STATE_NUM,
    [CHAR_CLASS_HASH] = LEXER_STATE_COMMENT,
    [CHAR_CLASS_MINUS] = LEXER_STATE_MINUS,
    [CHAR_CLASS_NEWLINE] = LEXER_STATE_START,
    [CHAR_CLASS_NULL] = LEXER_STATE_EOF,
    [CHAR_CLASS_OP] = LEXER_STATE_OP,
    [CHAR_CLASS_PUNCT] = LEXER_STATE_PUNCT,
    [CHAR_CLASS_SPACE] = LEXER_STATE_START,
  },
};

static Token (* const lexer_jump_table[LEXER_STATE_COUNT])(LexerState, char const *, i64) = {
  [LEXER_STATE_ID] = next_token__loop,
  [LEXER_STATE_MINUS] = next_token__loop,
  [LEXER_STATE_NUM] = next_token__loop,
  [LEXER_STATE_OP] = next_token__loop,
  [LEXER_STATE_COMMENT] = next_token__loop,
  [LEXER_STATE_START] = next_token__loop,
  [LEXER_STATE_COMPLETE_ID] = next_token__id,
  [LEXER_STATE_COMPLETE_NUM] = next_token__num,
  [LEXER_STATE_COMPLETE_OP] = next_token__op,
  [LEXER_STATE_EOF] = next_token__eof,
  [LEXER_STATE_ILLEGAL_CHAR] = next_token__illegal_char,
  [LEXER_STATE_PUNCT] = next_token__punct,
};
