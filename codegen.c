#include "chibi.h"

static int labelseq = 1;
static char *funcname;
static char *argregs[] = {"RDI", "RSI", "RDX", "RCX", "R8", "R9"};
static void gen(Node *node);

// Pushes the given node's address to the stack.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    printf("  lea rax, [rbp-%d]\n", node->var->offset);
    printf("  push rax\n");
    return;
  case ND_DEREF:
    gen(node->lhs);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void load(void) {
  printf("  pop rax\n");
  printf("  mov rax, [rax]\n");
  printf("  push rax\n");
}

static void store(void) {
  printf("  pop rdi\n");
  printf("  pop rax\n");
  printf("  mov [rax], rdi\n");
  printf("  push rdi\n");
}

// Generate code for a given node.
static void gen(Node *node) {
  switch (node->kind) {
  case ND_DEREF:
    gen(node->lhs);
    load();
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_NUM:
    printf("  push %ld\n", node->val);
    return;
  case ND_EXPR_STMT:
    gen(node->lhs);
    printf("  add rsp, 8\n");
    return;
  case ND_VAR:
    gen_addr(node);
    load();
    return;
  case ND_FUNC: {
    int stacknum = 0;
    for (Node *cur = node->funcarg; cur; stacknum++) {
      gen(cur);
      cur = cur->next;
    }

    while (stacknum > 0) {
      printf("  pop %s\n", argregs[stacknum - 1]);
      stacknum--;
    }
    // We need to align RSP to a 16 byte boundary before
    // calling a function because it is an ABI requirement.
    // RAX is set to 0 for variadic function.
    int seq = labelseq++;
    printf("  mov rax, rsp\n");
    printf("  and rax, 15\n");
    printf("  jnz .L.call.%d\n", seq);
    printf("  mov rax, 0\n");
    printf("  call %s\n", node->funcname);
    printf("  jmp .L.end.%d\n", seq);
    printf(".L.call.%d:\n", seq);
    printf("  sub rsp, 8\n");
    printf("  mov rax, 0\n");
    printf("  call %s\n", node->funcname);
    printf("  add rsp, 8\n");
    printf(".L.end.%d:\n", seq);
    printf("  push rax\n");
    return;
  }
  case ND_ASSIGN:
    gen_addr(node->lhs);
    gen(node->rhs);
    store();
    return;
  case ND_IF: {
    int seq = labelseq++;
    if (node->els) {
      gen(node->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je  .L.else.%d\n", seq);
      gen(node->then);
      printf("  jmp .L.end.%d\n", seq);
      printf(".L.else.%d:\n", seq);
      gen(node->els);
      printf(".L.end.%d:\n", seq);
    } else {
      gen(node->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je  .L.end.%d\n", seq);
      gen(node->then);
      printf(".L.end.%d:\n", seq);
    }
    return;
  }
  case ND_WHILE: {
    int seq = labelseq++;
    printf(".L.begin.%d:\n", seq);
    gen(node->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je  .L.end.%d\n", seq);
    gen(node->then);
    printf("  jmp .L.begin.%d\n", seq);
    printf(".L.end.%d:\n", seq);
    return;
  }
  case ND_FOR: {
    int seq = labelseq++;
    if (node->expr1)
      gen(node->expr1);
    printf(".L.begin.%d:\n", seq);
    if (node->expr2) {
      gen(node->expr2);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je  .L.end.%d\n", seq);
    }
    gen(node->stmt);
    if (node->expr3)
      gen(node->expr3);
    printf("  jmp .L.begin.%d\n", seq);
    printf(".L.end.%d:\n", seq);
    return;
  }
  case ND_BLOCK: {
    for (Node *n = node->body; n; n = n->next) {
      gen(n);
    }
    return;
  }
  case ND_RETURN:
    gen(node->lhs);
    printf("  pop rax\n");
    printf("  jmp .L.return.%s\n", funcname);
    return;
  }

  gen(node->lhs);
  gen(node->rhs);

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (node->kind) {
  case ND_ADD:
    printf("  add rax, rdi\n");
    break;
  case ND_SUB:
    printf("  sub rax, rdi\n");
    break;
  case ND_MUL:
    printf("  imul rax, rdi\n");
    break;
  case ND_DIV:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    break;
  case ND_EQ:
    printf("  cmp rax, rdi\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_NE:
    printf("  cmp rax, rdi\n");
    printf("  setne al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_LT:
    printf("  cmp rax, rdi\n");
    printf("  setl al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_LE:
    printf("  cmp rax, rdi\n");
    printf("  setle al\n");
    printf("  movzb rax, al\n");
    break;
  }

  printf("  push rax\n");
}

void codegen(Function *prog) {
  printf(".global %s\n", prog->funcname);
  printf("%s:\n", prog->funcname);
  funcname = prog->funcname;

  // Prologue
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", prog->stack_size);
  // Emit arg
  int argcnt = 0;
  for (VarList *list = prog->args; list; list = list->next) {
    Var *var = list->var;
    printf("  mov [rbp-%d], %s\n", var->offset, argregs[argcnt++]);
  }

  // Emit code
  for (Node *node = prog->node; node; node = node->next)
    gen(node);

  // Epilogue
  printf(".L.return.%s:\n", funcname);
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}
