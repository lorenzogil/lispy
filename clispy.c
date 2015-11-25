#include <stdio.h>   /* printf */
#include <stdlib.h>  /* free */

#include <editline/readline.h> /* readline */
#include <editline/history.h>  /* add_history */

#include "mpc.h"

/* Enumeration of possible lval types */
enum { LVAL_ERR, LVAL_NUM, LVAL_SYM, LVAL_SEXPR };

typedef struct lval {
  int type;
  long num;
  char* err;
  char* sym;
  int count;
  struct lval** cell;
} lval;


lval* lval_num (long x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval* lval_err (char* msg) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;
  v->err = malloc(strlen(msg) + 1);
  strcpy(v->err, msg);
  return v;
}

lval* lval_sym (char* sym) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(sym) + 1);
  strcpy(v->sym, sym);
  return v;
}

lval* lval_sexpr (void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_del (lval* v) {
  switch (v->type) {
  case LVAL_NUM:
    break;

  case LVAL_ERR:
    free(v->err);
    break;

  case LVAL_SYM:
    free(v->sym);
    break;

  case LVAL_SEXPR:
    for (int i=0; i < v->count; i++) {
      lval_del(v->cell[i]);
    }
    free(v->cell);
  }

  free(v);
}

lval* lval_read_num(mpc_ast_t* t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ? lval_num(x) : lval_err("invalid number");
}

lval* lval_add(lval* v, lval* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval *) * v->count);
  v->cell[v->count - 1] = x;
  return v;
}

lval* lval_read(mpc_ast_t* t) {
  if (strstr(t->tag, "number")) {
    return lval_read_num(t);
  }
  if (strstr(t->tag, "symbol")) {
    return lval_sym(t->contents);
  }

  /* If root (>) or sexpr then create empty list */
  lval* x = NULL;

  if (strcmp(t->tag, ">") == 0 || strstr(t->tag, "sexpr")) {
    x = lval_sexpr();
  }

  for (int i=0; i < t->children_num; i++) {
    char* contents = t->children[i]->contents;
    if (strcmp(contents, "(") == 0 ||
	strcmp(contents, ")") == 0 ||
	strcmp(contents, "{") == 0 ||
	strcmp(contents, "}") == 0) {
      continue;
    }

    if (strcmp(t->children[i]->tag, "regex") == 0) {
      continue;
    }
    x = lval_add(x, lval_read(t->children[i]));
						    
  }

  return x;
}

/* forward declaration */
void lval_print (lval* v);

void lval_expr_print (lval* v, char open, char close) {
  putchar(open);

  for (int i=0; i < v->count; i++) {
    lval_print(v->cell[i]);

    /* don't print trailing space if last element */
    if (i != (v->count - 1)) {
      putchar(' ');
    }
  }

  putchar(close);
}

void lval_print (lval* v) {
  switch (v->type) {
  case LVAL_NUM:
    printf("%li", v->num);
    break;

  case LVAL_ERR:
    printf("Error: %s", v->err);
    break;

  case LVAL_SYM:
    printf("%s", v->sym);
    break;

  case LVAL_SEXPR:
    lval_expr_print(v, '(', ')');
    break;
  }
}

void lval_println (lval* v) {
  lval_print(v);
  putchar('\n');
}

/*
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
*/

int main (int argc, char** argv) {

  /* Create individual parsers */
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Sexpr = mpc_new("sexpr");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  /* Define the parsers with a language */
  mpca_lang(MPCA_LANG_DEFAULT,
    "                                       \
      number : /-?[0-9]+/ ;                 \
      symbol: '+' | '-' | '*' | '/' ;       \
      sexpr: '(' <expr>* ')' ;              \
      expr: <number> | <symbol> | <sexpr> ; \
      lispy: /^/ <expr>* /$/ ;              \
    ", Number, Symbol, Sexpr, Expr, Lispy);

  puts("Lispy Version 0.0.0.0.5");
  puts("Press Ctrl+c to Exit\n");

  while (1) {

    char* input = readline("lispy> ");

    add_history(input);

    mpc_result_t r;
    if (mpc_parse ("<stdin>", input, Lispy, &r)) {
      lval* x = lval_read(r.output);
      lval_println(x);
      lval_del(x);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }

  mpc_cleanup(4, Number, Symbol, Sexpr, Expr, Lispy);

  return 0;
}
