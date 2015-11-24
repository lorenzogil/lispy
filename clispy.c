#include <stdio.h>   /* printf */
#include <stdlib.h>  /* free */

#include <editline/readline.h> /* readline */
#include <editline/history.h>  /* add_history */

#include "mpc.h"

/* Enumeration of possible lval types */
enum { LVAL_NUM, LVAL_ERR };

/* Enumeration of possible error types */
enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM };

typedef struct {
  int type;
  long num;
  int err;
} lval;


lval lval_num (long x) {
  lval v;
  v.type = LVAL_NUM;
  v.num = x;
  return v;
}

lval lval_err (int err_type) {
  lval v;
  v.type = LVAL_ERR;
  v.err = err_type;
  return v;
}

void lval_print (lval v) {
  switch (v.type) {
  case LVAL_NUM:
    printf("%li", v.num);
    break;

  case LVAL_ERR:
    switch (v.err) {
    case LERR_DIV_ZERO:
      printf("Error: Division by Zero!");
      break;
    case LERR_BAD_OP:
      printf("Error: Invalid Operator!");
      break;
    case LERR_BAD_NUM:
      printf("Error: Invalid Number!");
      break;
    }
    break;
  }
}

void lval_println (lval v) {
  lval_print(v);
  putchar('\n');
}

lval eval_op (lval x, char* op, lval y) {

  if (x.type == LVAL_ERR) {
    return x;
  }
  if (y.type == LVAL_ERR) {
    return y;
  }

  if (strcmp(op, "+") == 0) {
    return lval_num(x.num + y.num);
  } else if (strcmp(op, "-") == 0) {
    return lval_num(x.num - y.num);
  } else if (strcmp(op, "*") == 0) {
    return lval_num(x.num * y.num);
  } else if (strcmp(op, "/") == 0) {
    return y.num == 0 ? lval_err(LERR_DIV_ZERO) : lval_num(x.num / y.num);
  } else {
    return lval_err(LERR_BAD_OP);
  }
}

lval eval (mpc_ast_t* t) {

  if (strstr(t->tag, "number")) {
    errno = 0;
    long x = strtol(t->contents, NULL, 10);
    return errno != ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM);
  }

  char* op = t->children[1]->contents;

  lval x = eval(t->children[2]);

  int i = 3;
  while (strstr(t->children[i]->tag, "expr")) {
    x = eval_op(x, op, eval(t->children[i]));
    i++;
  }

  return x;
}

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

  puts("Lispy Version 0.0.0.0.4");
  puts("Press Ctrl+c to Exit\n");

  while (1) {

    char* input = readline("lispy> ");

    add_history(input);

    mpc_result_t r;
    if (mpc_parse ("<stdin>", input, Lispy, &r)) {
      lval result = eval(r.output);
      lval_println(result);
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
