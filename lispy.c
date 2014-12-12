#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <editline/readline.h>

#include "mpc/mpc.h"

enum lval_tag { LVAL_NUM, LVAL_SYM, LVAL_CONS, LVAL_NIL, LVAL_VEC,
                LVAL_FUNC, LVAL_PRIM, LVAL_ERR };

enum lval_errno { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_ARG,
                  LERR_BAD_ARITY, LERR_UNDEF, LERR_POOR_FORM };

typedef struct lval lval;
typedef struct lenv lenv;
typedef struct lclos lclos;
typedef lval* (*lprim)(int argc, lval**);

struct lclos {
  lval* body;
  lenv* env;
  char** formals;
  int count;
};

struct lval {
  enum lval_tag tag;
  union {
    long number;
    char* symbol;
    struct {
      int count;
      lval** elems;
    } vec;
    struct {
      lval* car;
      lval* cdr;
    } cons;
    lclos* func;
    lprim prim;
    int err;
  } value;
};

lval NIL = {LVAL_NIL};

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
  int len = strlen(characters);
  lval* result = malloc(sizeof(lval) + len);
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

lval* lval_vec(int count, lval** elems) {
  lval* result = malloc(sizeof(lval) + sizeof(lval*) * count);
  lval** reselems = (lval**)(result + 1);
  result->tag = LVAL_VEC;
  result->value.vec.count = count;
  result->value.vec.elems = reselems;
  memmove(reselems, elems, sizeof(lval*) * count);
  return result;
}

lval* lval_cons(lval* car, lval* cdr) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_CONS;
  result->value.cons.car = car;
  result->value.cons.cdr = cdr;
  return result;
}

lval* lval_prim(lprim f) {
  lval* result = malloc(sizeof(lval));
  result->tag = LVAL_PRIM;
  result->value.prim = f;
  return result;
}

lval* lval_clos(int count, char** formals, lenv* env, lval* body) {
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


static inline lval* car(lval* s) {
  if (s->tag != LVAL_CONS) {
    return lval_err(LERR_BAD_ARG);
  }
  return s->value.cons.car;
}

static inline lval* cdr(lval* s) {
  if (s->tag != LVAL_CONS) {
    return lval_err(LERR_BAD_ARG);
  }
  else {
    return s->value.cons.cdr;
  }
}

static inline lval* cddr(lval* s) {
  lval* cdrval = cdr(s);
  if (cdrval->tag != LVAL_CONS) {
    return lval_err(LERR_BAD_ARG);
  }
  else {
    return cdrval->value.cons.cdr;
  }
}

static inline lval* cadr(lval* s) {
  lval* cdrval = cdr(s);
  if (cdrval->tag != LVAL_CONS) {
    return lval_err(LERR_BAD_ARG);
  }
  else {
    return cdrval->value.cons.car;
  }
}

static inline int len(lval* s) {
  int l = 0;
  while (s->tag == LVAL_CONS) {
    l++;
    s = s->value.cons.cdr;
  }
  return l;
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
  else if (strstr(s->tag, "vector")) {
    // gross
    int count = 0;
    for (int i = 0; i < s->children_num; i++) {
      mpc_ast_t* s1 = s->children[i];
      if (strstr(s1->tag, "number") ||
          strstr(s1->tag, "symbol") ||
          strstr(s1->tag, "sexp")   ||
          strstr(s1->tag, "vector"))
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
    lval* res = lval_vec(count, children);
    free(children);
    return res;
  }
  else if (strstr(s->tag, "sexp")) {
    lval* list = &NIL;
    for (int i = s->children_num - 1; i >= 0; i--) {
      lval* child = read_lval(s->children[i]);
      if (child) {
        list = lval_cons(child, list);
      }
    }
    return list;
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
  case LVAL_VEC:
    putchar('[');
    for (int i = 0; i < v->value.vec.count; i++) {
      if (i > 0) putchar(' ');
      print_lval(v->value.vec.elems[i]);
    }
    putchar(']');
    break;
  case LVAL_NIL:
    printf("()");
  case LVAL_CONS:
    putchar('(');
    while (v->tag == LVAL_CONS) {
      print_lval(v);
      putchar(' ');
      v = v->value.cons.cdr;
    }
    if (v->tag != LVAL_NIL) {
      puts(". ");
      print_lval(v);
    }
    putchar(')');
    break;
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

lval* eval_cons(lenv*, lval*);
lval* eval_vec(lenv*, lval*);

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
  case LVAL_VEC:
    return eval_vec(env, e);
  case LVAL_NIL:
    return &NIL;
  case LVAL_CONS:
    return eval_cons(env, e);
  default:
    return lval_err(LERR_BAD_ARG);
  }
}

lval* eval_vec(lenv* env, lval* vec) {
  lval** vals = malloc(vec->value.vec.count * sizeof(lval*));
  for (int i = 0; i < vec->value.vec.count; i++) {
    vals[i] = eval(env, vec->value.vec.elems[i]);
  }
  lval* result = lval_vec(vec->value.vec.count, vals);
  free(vals);
  return result;
}

// assumes a list of expressions, anything else is bad syntax
lval* eval_progn(lenv* env, lval* exprs) {
  lval* val = NULL;
  while (exprs->tag == LVAL_CONS) {
    val = eval(env, exprs->value.cons.car);
    exprs = exprs->value.cons.cdr;
  }
  return val;
}

// Calling convention: stick 'em in an array. This means we have to
// count them first, but oh well. When there's a stack, they can go
// there.
lval* eval_application(lenv* env, lval* head, lval* exprs) {
  int count = len(exprs);
  lval** argv = malloc(sizeof(lval*) * count);
  int i = 0;
  while (exprs->tag == LVAL_CONS) {
    argv[i++] = eval(env, exprs->value.cons.car);
    exprs = cdr(exprs);
  }

  if (head->tag == LVAL_PRIM) {
    lprim f = head->value.prim;
    // arity??
    return f(count, argv);
  }
  else if (head->tag == LVAL_FUNC) {
    lclos* c = head->value.func;
    if (c->count != count) {
      return lval_err(LERR_BAD_ARITY);
    }
    lenv* newenv = lenv_extend(c->env, c->count, c->formals, argv);
    //    print_env(newenv);
    return eval_progn(newenv, c->body);
  }
  else {
    return lval_err(LERR_BAD_ARG);
  }
}

lval* eval_cons(lenv* env, lval* s) {
  lval* head = car(s);
  // special forms
  if (head->tag == LVAL_SYM) {
    if (strcmp(head->value.symbol, "lambda") == 0) {
      lval* args = cadr(s);
      if (args->tag != LVAL_VEC) {
        puts("args not a vector "); print_lval(args);
        return lval_err(LERR_POOR_FORM);
      }
      lval* body = cddr(s);
      char** formals = malloc(sizeof(char*) * args->value.vec.count);

      for (int i = 0; i < args->value.vec.count; i++) {
        lval* arg = args->value.vec.elems[i];
        if (arg->tag != LVAL_SYM) {
          puts("formal not a symbol "); print_lval(arg);
          return lval_err(LERR_POOR_FORM);
        }
        formals[i] = arg->value.symbol;
      }
      lval* closure = lval_clos(args->value.vec.count, formals, env, body);
      return closure;
    }
  }
  // ok treat it like a function
  head = eval(env, head);
  return eval_application(env, head, cdr(s));
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

int eval_root(lenv* toplevel, mpc_ast_t* root) {
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
int print_root(mpc_ast_t* root) {
  int multi = 0;
  for (int i = 0; i < root->children_num; i++) {
    lval* inval = read_lval(root->children[i]);
    if (inval) {
      if (multi) putchar(' ');
      print_lval(inval);
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
  mpc_parser_t* Vector = mpc_new("vector");
  mpc_parser_t* Program = mpc_new("program");
  
  mpca_lang(MPCA_LANG_DEFAULT, "                        \
    number   : /-?[0-9]+/ ;                             \
    symbol   : /[-a-zA-Z_0-9?!+*\\/]+/ ;                \
    expr     : <number> | <symbol> | <sexp> | <vector> ;\
    sexp     : '(' <expr>* ')' ;                        \
    vector   : '[' <expr>* ']' ;                        \
    program  : /^/ <expr>* /$/ ;                        \
  ", Number, Symbol, Expr, Sexp, Vector, Program);

  lenv* toplevel = init_toplevel();

  while (1) {
    char* in = readline(prompt);
    mpc_result_t r;
    if (mpc_parse("<stdin>", in, Program, &r)) {
      add_history(in);
      mpc_ast_t* root = r.output;
      //print_root(root);
      eval_root(toplevel, root);
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

  mpc_cleanup(6, Number, Symbol, Expr, Sexp, Vector, Program);

  return 0;
}
