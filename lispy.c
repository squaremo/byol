#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <editline/readline.h>

#include "mpc/mpc.h"

enum lval_tag { LVAL_NUM, LVAL_SYM, LVAL_SEXP,
                LVAL_FUNC, LVAL_PRIM, LVAL_ERR };

enum lval_errno { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_ARG,
                  LERR_BAD_ARITY, LERR_UNDEF, LERR_POOR_FORM };

typedef struct sexp sexp;
typedef struct lval lval;
typedef struct lenv lenv;
typedef struct lclos lclos;
typedef lval* (*lprim)(int argc, lval**);

struct sexp {
  lval** elems;
  int count;
};

struct lclos {
  sexp* body;
  lenv* env;
  char** formals;
  int count;
};

struct lval {
  enum lval_tag tag;
  union {
    long number;
    char* symbol;
    sexp sexp;
    lclos* func;
    lprim prim;
    int err; } value;
};

struct lenv {
  lenv* parent;
  int count;
  char** syms;
  lval** vals;
};

lenv* lenv_extend(lenv* parent, int count, char** names, lval** vals) {
  lenv* e = malloc(sizeof(lenv));
  e->syms = names;
  e->vals = vals;
  e->count = count;
  e->parent = parent;
  return e;
}

lval* lenv_lookup(lenv* env, char* name) {
  while (env != NULL) {
    for (int i = 0; i < env->count; i++) {
      if (strcmp(env->syms[i], name) == 0) {
        return env->vals[i];
      }
    }
    env = env->parent;
  }
  return NULL;
}

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

lval* lval_prim(lprim f) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_PRIM;
  result->value.prim = f;
  return result;
}

lval* lval_clos(int count, char** formals, lenv* env, sexp* body) {
  lclos* closure = malloc(sizeof(lclos));
  closure->count = count;
  closure->formals = formals;
  closure->body = body;
  closure->env = env;
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_FUNC;
  result->value.func = closure;
  return result;
}

int lval_free(lval* val) {
  switch (val->tag) {
  case LVAL_NUM:
  case LVAL_ERR:
  case LVAL_PRIM:
    break;
  case LVAL_FUNC:
    free(val->value.func);
    break;
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

char* err_msg(int err) {
  switch (err) {
  case LERR_DIV_ZERO:
    return "Divide by zero difficulty";
  case LERR_BAD_OP:
    return "Unknown operation";
  case LERR_BAD_ARG:
    return "Bad argument";
  case LERR_BAD_ARITY:
    return "Arity mismatch";
  case LERR_UNDEF:
    return "Undefined variable";
  case LERR_POOR_FORM:
    return "Poor form";
  }
  return "I have never seen this before in my life";
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
    printf("<error %s>", err_msg(v->value.err));
    break;
  case LVAL_FUNC:
  case LVAL_PRIM:
    printf("<function>");
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

void print_env(lenv* env) {
  while (env != NULL) {
    puts("env");
    for (int i = 0; i < env->count; i++) {
      printf("%s = ", env->syms[i]); print_lval(env->vals[i]); putchar('\n');
    }
    env = env->parent;
  }
}

lval* eval_sexp(lenv*, lval*);

lval* eval(lenv* env, lval* e) {
  switch (e->tag) {
  case LVAL_NUM:
    return e;
  case LVAL_SYM:
    {
      lval* v = lenv_lookup(env, e->value.symbol);
      if (v == NULL) return lval_err(LERR_UNDEF);
      else return v;
    }
  case LVAL_SEXP:
    return eval_sexp(env, e);
  default:
    return NULL;
  }
}

lval* eval_progn(lenv* env, sexp* exprs) {
  lval* val = NULL;
  for (int i = 0; i < exprs->count; i++) {
    val = eval(env, exprs->elems[i]);
  }
  return val;
}

lval* eval_application(lenv* env, lval* head, int count, lval** exprs) {
  if (head->tag == LVAL_PRIM) {
    lprim f = head->value.prim;
    lval** argv = malloc(sizeof(lval*) * count);
    for (int i = 0; i < count; i++) {
      argv[i] = eval(env, exprs[i]);
    }
    return f(count, argv);
  }
  else if (head->tag == LVAL_FUNC) {
    lclos* c = head->value.func;
    if (c->count != count) {
      return lval_err(LERR_BAD_ARITY);
    }
    lval** argv = malloc(sizeof(lval*) * count);
    for (int i = 0; i < count; i++) {
      argv[i] = eval(env, exprs[i]);
    }
    lenv* newenv = lenv_extend(c->env, c->count, c->formals, argv);
    //    print_env(newenv);
    return eval_progn(newenv, c->body);
  }
  else {
    return lval_err(LERR_BAD_ARG);
  }
}

static inline lval* car(lval* s) {
  if (s->value.sexp.count < 1) {
    return lval_err(LERR_BAD_ARG);
  }
  return s->value.sexp.elems[0];
}

static inline lval* cdr(lval* s) {
  if (s->value.sexp.count < 1) {
    return lval_err(LERR_BAD_ARG);
  }
  else {
    return lval_sexp(s->value.sexp.count - 1,
                     s->value.sexp.elems + 1);
  }
}

static inline lval* cddr(lval* s) {
  if (s->value.sexp.count < 2) {
    return lval_err(LERR_BAD_ARG);
  }
  else {
    return lval_sexp(s->value.sexp.count - 2,
                     s->value.sexp.elems + 2);
  }
}

static inline lval* cadr(lval* s) {
  if (s->value.sexp.count < 2) {
    return lval_err(LERR_BAD_ARG);
  }
  else {
    return s->value.sexp.elems[1];
  }
}

lval* eval_sexp(lenv* env, lval* s) {
  if (s->value.sexp.count == 0) {
    return s;
  }
  else {
    lval* head = car(s);
    // special forms
    if (head->tag == LVAL_SYM) {
      if (strcmp(head->value.symbol, "lambda") == 0) {
        if (s->value.sexp.count < 3) {
          return lval_err(LERR_POOR_FORM);
        }
        lval* args = cadr(s);
        if (args->tag != LVAL_SEXP) {
          return lval_err(LERR_BAD_ARG);
        }
        lval* body = cddr(s);
        int count = args->value.sexp.count;
        char** formals = malloc(sizeof(char*) * count);
        for (int i = 0; i < count; i++) {
          lval* arg = args->value.sexp.elems[i];
          if (arg->tag != LVAL_SYM) {
            return lval_err(LERR_BAD_ARG);
          }
          formals[i] = arg->value.symbol;
        }
        lval* closure = lval_clos(count, formals, env, &(body->value.sexp));
        return closure;
      }
    }
    // ok treat it like a function
    head = eval(env, head);
    lval** args = s->value.sexp.elems + 1;
    return eval_application(env, head, s->value.sexp.count - 1, args);
  }
}


lval* prim_plus(int argc, lval** argv) {
  long result = 0;
  for (int i = 0; i < argc; i++) {
    lval* arg = argv[i];
    if (arg->tag != LVAL_NUM) {
      return lval_err(LERR_BAD_ARG);
    }
    result += arg->value.number;
  }
  return lval_num(result);
}

lenv* init_toplevel() {
  int c = 1;
  char** names = malloc(c * sizeof(char*));
  lval** prims = malloc(c * sizeof(lval*));

  names[0] = "+"; prims[0] = lval_prim(&prim_plus);

  return lenv_extend(NULL, 1, names, prims) ;
}

int eval_and_free_root(lenv* toplevel, mpc_ast_t* root) {
  int multi = 0;
  for (int i = 0; i < root->children_num; i++) {
    lval* inval = read_lval(root->children[i]);
    if (inval) {
      if (multi) putchar('\n');
      lval* res = eval(toplevel, inval);
      print_lval(res);
      multi = 1;
    }
  }
  return 0;
}

// We can get either an expression, or program
int print_and_free_root(mpc_ast_t* root) {
  int multi = 0;
  for (int i = 0; i < root->children_num; i++) {
    lval* inval = read_lval(root->children[i]);
    if (inval) {
      if (multi) putchar(' ');
      print_lval(inval);
      lval_free(inval);
      multi = 1;
    }
  }
  return 0;
}

int main(int argc, char** argv) {
  static char* prompt = "lispy> ";

  // don't buffer stdout
  setbuf(stdout, NULL);

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

  lenv* toplevel = init_toplevel();

  while (1) {
    char* in = readline(prompt);
    mpc_result_t r;
    if (mpc_parse("<stdin>", in, Program, &r)) {
      add_history(in);
      mpc_ast_t* root = r.output;
      //print_and_free_root(root);
      eval_and_free_root(toplevel, root);
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
