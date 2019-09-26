#include "chibi.h"

// All local variable instances created during parsing are
// accumulated to this list.
VarList *locals;

// Arguments
VarList *args;

// Find a local variable by name.
static Var *find_var(Token *tok) {
  for (VarList *list = locals; list; list = list->next) {
    Var *var = list->var;
    if (strlen(var->name) == tok->len && !strncmp(tok->str, var->name, tok->len))
      return var;
  }
  return NULL;
}

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = expr;
  return node;
}

static Node *new_num(long val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  return node;
}

static Node *new_var_node(Var *var, Token *tok) {
  Node *node = new_node(ND_VAR, tok);
  node->var = var;
  return node;
}

static Var *new_lvar(char *name) {
  Var *var = calloc(1, sizeof(Var));
  var->name = name;

  VarList *list = calloc(1, sizeof(VarList));
  list->next = locals;
  list->var = var;
  locals = list;
  return list->var;
}

static Node *stmt(void);
static Node *expr(void);
static Node *assign(void);
static Node *equality(void);
static Node *relational(void);
static Node *add(void);
static Node *mul(void);
static Node *unary(void);
static Node *primary(void);
static Function *function();

// program = function function*
Function *program(void) {
  Function headfunc = {};
  Function *curfunc = &headfunc;
  while (!at_eof()) {
    curfunc->next = function();
    curfunc = curfunc->next;
  }
  return headfunc.next;
}

Var *new_arg_var() {
  Token *tok = consume_ident();
  if (!tok)
    return NULL;
  return new_lvar(strndup(tok->str, tok->len));
}

// def-func-args = ident ( "," ident )*
VarList *def_func_args() {
  if (consume(")"))
    return NULL;
  VarList *head = calloc(1, sizeof(VarList));
  head->var = new_arg_var();
  VarList *cur = head;
  while(consume(",")) {
    cur->next = calloc(1, sizeof(VarList));
    cur->next->var = new_arg_var();
    cur = cur->next;
  }
  expect(")");
  return head;
}

// function = ident "(" def-func-args? ")" "{" stmt* "}"
Function *function() {
  Token *tok = consume_ident();
  expect("(");
  VarList *args = def_func_args();
  expect("{");

  Node head = {};
  Node *cur = &head;
  while (!consume("}")) {
    cur->next = stmt();
    cur = cur->next;
  }

  Function *prog = calloc(1, sizeof(Function));
  prog->node = head.next;
  prog->locals = locals;
  prog->args = args;
  prog->funcname = strndup(tok->str, tok->len);
  return prog;
}

static Node *read_expr_stmt(void) {
  Token *tok = token;
  return new_unary(ND_EXPR_STMT, expr(), tok);
}

// stmt = "return" expr ";"
//      | "{" stmt* "}"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "while" "(" expr ")" stmt
//      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//      | expr ";"
static Node *stmt(void) {
  Token *tok;
  if (tok = consume("return")) {
    Node *node = new_unary(ND_RETURN, expr(), tok);
    expect(";");
    return node;
  }

  if (tok = consume("if")) {
    Node *node = new_node(ND_IF, tok);
    expect("(");
    node->cond = expr();
    expect(")");
    node->then = stmt();
    if (consume("else"))
      node->els = stmt();
    return node;
  }

  if (tok = consume("while")) {
    Node *node = new_node(ND_WHILE, tok);
    expect("(");
    node->cond = expr();
    expect(")");
    node->then = stmt();
    return node;
  }

  if (tok = consume("for")) {
    Node *node = new_node(ND_FOR, tok);
    expect("(");
    if (consume(";") == false) {
      node->expr1 = expr();
      expect(";");
    }
    if (consume(";") == false) {
      node->expr2 = expr();
      expect(";");
    }
    if (consume(")") == false) {
      node->expr3 = expr();
      expect(")");
    }
    node->stmt = stmt();
    return node;
  }

  if (tok = consume("{")) {
    Node head = {};
    Node *cur = &head;
    while (!consume("}")) {
      cur->next = stmt();
      cur = cur->next;
    }

    Node *node = new_node(ND_BLOCK, tok);
    node->body = head.next;
    return node;
  }


  Node *node = read_expr_stmt();
  expect(";");
  return node;
}

// expr = assign
static Node *expr(void) {
  return assign();
}

// assign = equality ("=" assign)?
static Node *assign(void) {
  Node *node = equality();
  Token *tok;
  if (tok = consume("="))
    node = new_binary(ND_ASSIGN, node, assign(), tok);
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(void) {
  Node *node = relational();
  Token *tok;

  for (;;) {
    if (tok = consume("=="))
      node = new_binary(ND_EQ, node, relational(), tok);
    else if (tok = consume("!="))
      node = new_binary(ND_NE, node, relational(), tok);
    else
      return node;
  }
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
static Node *relational(void) {
  Node *node = add();
  Token *tok;

  for (;;) {
    if (tok = consume("<"))
      node = new_binary(ND_LT, node, add(), tok);
    else if (tok = consume("<="))
      node = new_binary(ND_LE, node, add(), tok);
    else if (tok = consume(">"))
      node = new_binary(ND_LT, add(), node, tok);
    else if (tok = consume(">="))
      node = new_binary(ND_LE, add(), node, tok);
    else
      return node;
  }
}

// add = mul ("+" mul | "-" mul)*
static Node *add(void) {
  Node *node = mul();
  Token *tok;

  for (;;) {
    if (tok = consume("+"))
      node = new_binary(ND_ADD, node, mul(), tok);
    else if (tok = consume("-"))
      node = new_binary(ND_SUB, node, mul(), tok);
    else
      return node;
  }
}

// mul = unary ("*" unary | "/" unary)*
static Node *mul(void) {
  Node *node = unary();
  Token *tok;

  for (;;) {
    if (tok = consume("*"))
      node = new_binary(ND_MUL, node, unary(), tok);
    else if (tok = consume("/"))
      node = new_binary(ND_DIV, node, unary(), tok);
    else
      return node;
  }
}

// unary = ("+" | "-" | "*" | "&")? unary
//       | primary
static Node *unary(void) {
  Token *tok;
  if (tok = consume("+"))
    return unary();
  if (tok = consume("-"))
    return new_binary(ND_SUB, new_num(0, tok), unary(), tok);
  if (tok = consume("*"))
    return new_unary(ND_DEREF, unary(), tok);
  if (tok = consume("&"))
    return new_unary(ND_ADDR, unary(), tok);
  return primary();
}

// func-args = "(" ( assign ( "," assign )* )? ")"
static Node *func_args(void) {
  Node head = {};
  Node *cur = &head;
  for (int i = 0; (!consume(")") | (i == 5)); i++) {
    cur->next = assign();
    cur = cur->next;
    consume(",");
  }
  return head.next;
}

// primary = "(" expr ")" | ident func-args? | num
static Node *primary(void) {
  if (consume("(")) {
    Node *node = expr();
    expect(")");
    return node;
  }

  Token *tok;
  if (tok = consume_ident()) {
    if (consume("(")) {
      Node *funcall = new_node(ND_FUNC, tok);
      funcall->funcname = strndup(tok->str, tok->len);
      funcall->funcarg = func_args();
      return funcall;
    } else {
      Var *var = find_var(tok);
      if (!var)
        var = new_lvar(strndup(tok->str, tok->len));
      return new_var_node(var, tok);
    }
  }

  tok = token;
  if (tok->kind != TK_NUM)
    error_tok(tok, "expected expression");
  return new_num(expect_number(), tok);
}
