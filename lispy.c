#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <editline/readline.h>

#include "mpc/mpc.h"

// ==== values

enum lval_tag { LVAL_NUM, LVAL_SYM, LVAL_CONS, LVAL_NIL, LVAL_VEC,
                LVAL_FUNC, LVAL_PRIM, LVAL_ERR };

enum lval_errno { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_ARG,
                  LERR_BAD_ARITY, LERR_UNDEF, LERR_POOR_FORM };

typedef struct lval lval;
typedef struct lclos lclos;
typedef lval* (*lprim)(int argc, lval**);

struct lclos {
  lval* body;
  lval* env;
  lval* formals;
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

static inline int lsym_cmp(lval* a, lval* b) {
  return strcmp(a->value.symbol, b->value.symbol);
}

static inline int lvec_count(lval* vec) {
  return vec->value.vec.count;
}

static inline void lvec_set(lval* vec, int i, lval* val) {
  vec->value.vec.elems[i] = val;
}

static inline lval* lvec_get(lval* vec, int i) {
  return vec->value.vec.elems[i];
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

lval* lval_vec(int count) {
  lval* result = malloc(sizeof(lval) + sizeof(lval*) * count);
  lval** reselems = (lval**)(result + 1);
  result->tag = LVAL_VEC;
  result->value.vec.count = count;
  result->value.vec.elems = reselems;
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

lval* lval_clos(lval* formals, lval* env, lval* body) {
  lclos* closure = malloc(sizeof(lclos));
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

// === environments

lval* lenv_extend(lval* parent, lval* names, lval** vals) {
  int count = lvec_count(names);
  lval* e = lval_vec(count + 2);
  lvec_set(e, 0, parent);
  lvec_set(e, 1, names);
  for (int i = 0; i < count; i++) {
    lvec_set(e, i + 2, vals[i]);
  }
  return e;
}

lval* lenv_lookup(lval* env, lval* name) {
  while (env != NULL) {
    lval* names = lvec_get(env, 1);
    int count = lvec_count(names);
    for (int i = 0; i < count; i++) {
      if (lsym_cmp(lvec_get(names, i), name) == 0) {
        return lvec_get(env, i + 2);
      }
    }
    env = lvec_get(env, 0);
  }
  return NULL;
}


// ======= reading and printing

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
    int childi = 0;
    lval* res = lval_vec(count);
    for (int i = 0; i < s->children_num; i++) {
      lval* child = read_lval(s->children[i]);
      if (child) {
        lvec_set(res, childi++, child);
      }
    }
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
    for (int i = 0; i < lvec_count(v); i++) {
      if (i > 0) putchar(' ');
      print_lval(lvec_get(v, i));
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

void print_env(lval* env) {
  while (env != NULL) {
    lval* names = lvec_get(env, 1);
    puts("env");
    for (int i = 0; i < lvec_count(names); i++) {
      print_lval(lvec_get(names, i)); puts(" = ");
      print_lval(lvec_get(env, i + 2));
      putchar('\n');
    }
    env = lvec_get(env, 0);
  }
}

lval* eval_cons(lval*, lval*);
lval* eval_vec(lval*, lval*);

lval* eval(lval* env, lval* e) {
  switch (e->tag) {
  case LVAL_NUM:
    return e;
  case LVAL_SYM:
    {
      lval* v = lenv_lookup(env, e);
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

lval* eval_vec(lval* env, lval* vec) {
  lval* res = lval_vec(lvec_count(vec));
  for (int i = 0; i < lvec_count(vec); i++) {
    lvec_set(res, i, eval(env, lvec_get(vec, i)));
  }
  return res;
}

// assumes a list of expressions, anything else is bad syntax
lval* eval_progn(lval* env, lval* exprs) {
  lval* val = NULL;
  while (exprs->tag == LVAL_CONS) {
    val = eval(env, car(exprs));
    exprs = cdr(exprs);
  }
  return val;
}

// Calling convention: stick 'em in an array. This means we have to
// count them first, but oh well. When there's a stack, they can go
// there.
lval* eval_application(lval* env, lval* head, lval* exprs) {
  lval* res;
  int count = len(exprs);
  lval** argv = malloc(sizeof(lval*) * count);
  int i = 0;
  while (exprs->tag == LVAL_CONS) {
    argv[i++] = eval(env, car(exprs));
    exprs = cdr(exprs);
  }

  if (head->tag == LVAL_PRIM) {
    lprim f = head->value.prim;
    // arity??
    res = f(count, argv);
  }
  else if (head->tag == LVAL_FUNC) {
    lclos* c = head->value.func;
    if (lvec_count(c->formals) != count) {
      res = lval_err(LERR_BAD_ARITY);
    }
    else {
      lval* newenv = lenv_extend(c->env, c->formals, argv);
      res = eval_progn(newenv, c->body);
    }
  }
  else {
    res = lval_err(LERR_BAD_ARG);
  }
  free(argv);
  return res;
}

lval* eval_cons(lval* env, lval* s) {
  lval* head = car(s);
  // special forms
  if (head->tag == LVAL_SYM) {
    if (strcmp(head->value.symbol, "lambda") == 0) {
      lval* args = cadr(s);
      if (args->tag != LVAL_VEC) {
        //puts("args not a vector "); print_lval(args);
        return lval_err(LERR_POOR_FORM);
      }
      lval* body = cddr(s);

      // check we have all symbols
      for (int i = 0; i < lvec_count(args); i++) {
        if ((lvec_get(args, i))->tag != LVAL_SYM) {
          //puts("formal not a symbol "); print_lval(lvec_get(args, i));
          return lval_err(LERR_POOR_FORM);
        }
      }
      lval* closure = lval_clos(args, env, body);
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

lval* init_toplevel() {
  int c = 1;

  lval* names = lval_vec(c);
  lval** prims = malloc(c * sizeof(lval*));

  lvec_set(names, 0, lval_sym("+")); prims[0] = lval_prim(&prim_plus);
  return lenv_extend(NULL, names, prims) ;
}

int eval_root(lval* toplevel, mpc_ast_t* root) {
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

  puts("Lispy version 0.0.2");
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

  lval* toplevel = init_toplevel();

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
