#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <editline/readline.h>

#include "mpc/mpc.h"

enum { LVAL_NUM, LVAL_SYM, LVAL_SEXP, LVAL_ERR };

enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_ARG };

typedef struct sexp sexp;
typedef struct lval lval;

struct sexp {
  lval** elems;
  int count;
};

struct lval {
  int tag;
  union {
    long number;
    char* symbol;
    sexp sexp;
    int err; } value;
};

lval* lval_num(long value) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_NUM;
  result->value.number = value;
  return result;
}

lval* lval_sym(char* characters) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_SYM;
  result->value.symbol = malloc(strlen(characters) + 1);
  strcpy(result->value.symbol, characters);
  return result;
}

lval* lval_err(int errtype) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_ERR;
  result->value.err = errtype;
  return result;
}

lval* lval_sexp(int count, lval** elems) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_SEXP;
  result->value.sexp.count = count;
  result->value.sexp.elems = elems;
  return result;
}

int lval_free(lval* val) {
  switch (val->tag) {

  case LVAL_SYM:
    free(val->value.symbol);
    break;
  case LVAL_SEXP:
    for (int i = 0; i < val->value.sexp.count; i++) {
      lval_free(val->value.sexp.elems[i]);
    }
    free(val->value.sexp.elems);
    break;
  }
  free(val);
  return 0;
}

lval* read_number(mpc_ast_t* n) {
  errno = 0;
  long x = strtol(n->contents, NULL, 10);
  // ignore for now
  return lval_num(x);
}

lval* read_lval(mpc_ast_t* s) {
  if (strstr(s->tag, "number")) {
    return read_number(s);
  }
  else if (strstr(s->tag, "symbol")) {
    return lval_sym(s->contents);
  }
  else if (strstr(s->tag, "sexp")) {
    int count = 0;
    for (int i = 0; i < s->children_num; i++) {
      mpc_ast_t* s1 = s->children[i];
      if (strstr(s1->tag, "number") ||
          strstr(s1->tag, "symbol") ||
          strstr(s1->tag, "sexp"))
        count++;
    }
    lval** children = malloc(sizeof(lval) * count);
    int childi = 0;
    for (int i = 0; i < s->children_num; i++) {
      lval* child = read_lval(s->children[i]);
      if (child) {
        children[childi++] = child;
      }
    }
    return lval_sexp(count, children);
  }
  return NULL;
}

int print_lval(lval* v) {
  switch (v->tag) {
  case LVAL_NUM:
    printf("%li", v->value.number);
    break;
  case LVAL_SYM:
    printf("%s", v->value.symbol);
    break;
  case LVAL_ERR:
    printf("<error %d>", v->value.err);
    break;
  case LVAL_SEXP:
    putchar('(');
    for (int i = 0; i < v->value.sexp.count; i++) {
      if (i > 0) putchar(' ');
      print_lval(v->value.sexp.elems[i]);
    }
    putchar(')');
  }
  return 0;
}

// We can get either an expression, or program
int print_and_free_root(mpc_ast_t* root) {
  for (int i = 0; i < root->children_num; i++) {
    lval* inval = read_lval(root->children[i]);
    if (inval) {
      print_lval(inval);
      lval_free(inval);
      putchar(' ');
    }
  }
  return 0;
}

int main(int argc, char** argv) {
  static char* prompt = "lispy> ";

  puts("Lispy version 0.0.1");
  puts("Press Ctrl+C to exit");

  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Sexp = mpc_new("sexp");
  mpc_parser_t* Program = mpc_new("program");
  
  mpca_lang(MPCA_LANG_DEFAULT, "                        \
    number   : /-?[0-9]+/ ;                             \
    symbol   : /[-a-zA-Z_0-9?!+*\\/]+/ ;                \
    expr     : <number> | <symbol> | <sexp> ;           \
    sexp     : '(' <expr>* ')' ;                        \
    program  : /^/ <expr>* /$/ ;                        \
  ", Number, Symbol, Expr, Sexp, Program);

  while (1) {
    char* in = readline(prompt);
    mpc_result_t r;
    if (mpc_parse("<stdin>", in, Program, &r)) {
      add_history(in);
      mpc_ast_t* root = r.output;
      print_and_free_root(root);
      putchar('\n');
      //mpc_ast_print(r.output);
      mpc_ast_delete(r.output);
    }
    else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(in);
  }

  mpc_cleanup(5, Number, Symbol, Expr, Sexp, Program);

  return 0;
}
