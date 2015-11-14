#include <stdio.h>   /* printf */
#include <stdlib.h>  /* free */

#include <editline/readline.h> /* readline */
#include <editline/history.h>  /* add_history */

#include "mpc.h"


int main (int argc, char** argv) {

  /* Create individual parsers */
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Operator = mpc_new("operator");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  /* Define the parsers with a language */
  mpca_lang(MPCA_LANG_DEFAULT,
    "                                               \
      number : /-?[0-9]+/ ;                         \
      operator: '+' | '-' | '*' | '/' ;             \
      expr: <number> | '(' <operator> <expr>+ ')' ; \
      lispy: /^/ <operator> <expr>+ /$/ ;           \
    ", Number, Operator, Expr, Lispy);

  puts("Lispy Version 0.0.0.0.2");
  puts("Press Ctrl+c to Exit\n");

  while (1) {

    char* input = readline("lispy> ");

    add_history(input);

    mpc_result_t r;
    if (mpc_parse ("<stdin>", input, Lispy, &r)) {
      mpc_ast_print(r.output);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }

  mpc_cleanup(4, Number, Operator, Expr, Lispy);

  return 0;
}
