#include "chibi.h"

int main(int argc, char **argv) {
  if (argc != 2)
    error("%s: invalid number of arguments", argv[0]);

  // Tokenize and parse.
  user_input = argv[1];
  token = tokenize();

  printf(".intel_syntax noprefix\n");
  for (Function *prog = program(); prog; prog = prog->next) {
    // Assign offsets to local variables.
    int offset = 0;
    for (Var *var = prog->locals; var; var = var->next) {
      offset += 8;
      var->offset = offset;
    }
    prog->stack_size = offset;
  
    // Traverse the AST to emit assembly.
    codegen(prog);
  }

  return 0;
}
