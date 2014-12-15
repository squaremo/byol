#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include <editline/readline.h>

#include "mpc/mpc.h"

// ==== values

enum lval_tag { LVAL_FWD, LVAL_NUM, LVAL_SYM, LVAL_CONS,
                LVAL_NIL, LVAL_VEC, LVAL_FUNC, LVAL_PRIM,
                LVAL_ERR };

typedef intptr_t obj;

typedef struct {
  uint64_t tag:8;
  uint64_t size:56;
} header;

typedef long number;
typedef char symbol;
typedef char err;
typedef struct {
  int count;
} vector;
typedef obj (*prim_fun)(int, vector*);
typedef prim_fun prim;
typedef struct {
  obj car;
  obj cdr;
} cons;
typedef struct {
  vector* env;
  vector* formals;
  cons* body;
} closure;
typedef char nil;

typedef struct {
  header hdr;
  obj obj;
} fwd;

static inline enum lval_tag obj_tag(obj val) {
  return ((header*)val - 1)->tag;
}
#define tag(v) (obj_tag((obj)(v)))

static inline int obj_size(obj val) {
  return ((header*)val - 1)->size;
}

static inline int sym_cmp(symbol* a, symbol* b) {
  return strcmp(a, b);
}

static inline int vec_count(vector* vec) {
  return vec->count;
}

static inline void vec_set(vector* vec, int i, obj val) {
  ((obj*)(vec + 1))[i] = val;
}

static inline obj vec_get(vector* vec, int i) {
  return ((obj*)(vec + 1))[i];
}

static inline int round_to_word(int n) {
  return (n + (sizeof(intptr_t) - 1)) & ~(sizeof(intptr_t) - 1);
}

// garbage collection

#define STACK_SIZE 2048
#define HEAP_SIZE 2048

obj** stack;
int stack_ptr = 0;

void* fromspace;
void* tospace;

intptr_t next;
intptr_t limit;

void gc_flip() {
  void* tmp = fromspace;
  fromspace = tospace;
  tospace = tmp;
  next = (intptr_t)fromspace;
  limit = (intptr_t)fromspace + HEAP_SIZE;
}

void gc_init() {
  stack = malloc(sizeof(obj*) * STACK_SIZE);
  fromspace = malloc(HEAP_SIZE);
  tospace = malloc(HEAP_SIZE);
  gc_flip();
}

void gc_push_root(obj* root) {
  stack[stack_ptr++] = root;
  assert(stack_ptr < STACK_SIZE);
}

void gc_pop_roots(int entries) {
  stack_ptr -= entries;
  assert(stack_ptr >= 0);
}

#define KEEP(var) gc_push_root((obj*)&(var))
#define FORGET(num) gc_pop_roots(num)

obj gc_copy(obj o) {
  if (tag(o) == LVAL_FWD) {
    fwd* f = (fwd*)o;
    return f->obj;
  }
  else {
    int bytes = sizeof(header) + obj_size(o) * sizeof(intptr_t);
    fwd* from = (fwd*)((header*)o - 1);
    memmove((void*)next, (void*)from, bytes);
    obj to = (obj)((header*)next + 1);
    from->hdr.tag = LVAL_FWD;
    from->obj = to;
    next += bytes;
    return to;
  }
}

void gc() {
  gc_flip();
  // phase one: copy over everything on the stack
  for (int i = 0; i < stack_ptr; i++) {
    *stack[i] = gc_copy(*stack[i]);
  }
  // phase two: copy over things reachable from anything we copied earlier
  intptr_t todo = (intptr_t)fromspace;
  while (todo < next) {
    header* h = (header*)todo;
    enum lval_tag tag = h->tag;
    assert(tag != LVAL_FWD);
    int bytes = h->size;
    switch (tag) {
    case LVAL_CONS:
      {
        cons* c = (cons*)(h + 1);
        c->car = gc_copy(c->car);
        c->cdr = gc_copy(c->cdr);
        break;
      }
    case LVAL_VEC:
      {
        vector* v = (vector*)(h + 1);
        int count = vec_count(v);
        for (int i = 0; i < count; i++) {
          vec_set(v, i, gc_copy(vec_get(v, i)));
        }
        break;
      }
    case LVAL_FUNC:
      {
        closure* c = (closure*)(h + 1);
        c->formals = (vector*)gc_copy((obj)c->formals);
        c->env = (vector*)gc_copy((obj)c->env);
        c->body = (cons*)gc_copy((obj)c->body);
        break;
      }
    default:
      // nothing else has obj fields
      break;
    }
    todo += bytes;
  }
}

static inline obj alloc_obj(enum lval_tag tag, int size) {
  // sizeof(header) will be padded anyway, so the sum will be rounded
  assert(tag != LVAL_FWD);
  assert(size > 0);
  int bytes = sizeof(header) + round_to_word(size);
  if (next + bytes > limit) {
    gc();
    return alloc_obj(tag, size);
  }
  header* h = (header*)next;
  h->tag = tag;
  // NB size does not include header
  h->size = round_to_word(size) / sizeof(intptr_t);
  next += bytes;
  return (obj)(h + 1);
}

// === constructing values

number* make_num(long value) {
  number* result = (number*)alloc_obj(LVAL_NUM, sizeof(number));
  *result = value;
  return result;
}

symbol* make_sym(char* characters) {
  int size = strlen(characters) + 1;
  symbol* result = (symbol*)alloc_obj(LVAL_SYM, size);
  strcpy(result, characters);
  return result;
}

err* make_err(char* msg) {
  err* result = (err*)alloc_obj(LVAL_ERR, strlen(msg) + 1);
  strcpy(result, msg);
  return result;
}

vector* make_vec(int count) {
  int size = sizeof(vector) + sizeof(obj) * count;
  vector* result = (vector*)alloc_obj(LVAL_VEC, size);
  result->count = count;
  return result;
}

cons* make_cons(obj car, obj cdr) {
  KEEP(car); KEEP(cdr);
  cons* result = (cons*)alloc_obj(LVAL_CONS, sizeof(cons));
  FORGET(2);
  result->car = car;
  result->cdr = cdr;
  return result;
}

nil* make_nil() {
  nil* result = (nil*)alloc_obj(LVAL_NIL, 4);
  strcpy(result, "nil");
  return result;
}

prim* make_prim(prim_fun f) {
  prim* result = (prim*)alloc_obj(LVAL_PRIM, sizeof(prim));
  *result = f;
  return result;
}

closure* make_clos(vector* formals, vector* env, cons* body) {
  KEEP(formals); KEEP(env); KEEP(body);
  closure* result = (closure*)alloc_obj(LVAL_FUNC, sizeof(closure));
  FORGET(3);
  result->formals = formals;
  result->env = env;
  result->body = body;
  return result;
}

static inline obj car(cons* s) {
  if (tag(s) != LVAL_CONS) {
    return (obj)make_err("Tried to get car of non-list");
  }
  return s->car;
}

static inline obj cdr(cons* s) {
  if (tag(s) != LVAL_CONS) {
    return (obj)make_err("Tried to get cdr of non-list");
  }
  else {
    return s->cdr;
  }
}

static inline obj cddr(cons* s) {
  cons* cdrval = (cons*)cdr(s);
  if (tag(cdrval) != LVAL_CONS) {
    return (obj)make_err("Tried to get cddr of not long enough list");
  }
  else {
    return cdrval->cdr;
  }
}

static inline obj cadr(cons* s) {
  cons* cdrval = (cons*)cdr(s);
  if (tag(cdrval) != LVAL_CONS) {
    return (obj)make_err("Tried to get cadr of not long enough list");
  }
  else {
    return cdrval->car;
  }
}

static inline int cons_len(cons* s) {
  int l = 0;
  while (tag(s) == LVAL_CONS) {
    l++;
    s = (cons*)s->cdr;
  }
  return l;
}

// === environments

vector* env_extend(vector* parent, vector* names, vector* vals) {
  int count = vec_count(names);
  KEEP(parent); KEEP(names); KEEP(vals);
  vector* e = make_vec(count + 2);
  FORGET(3);
  vec_set(e, 0, (obj)parent);
  vec_set(e, 1, (obj)names);
  for (int i = 0; i < count; i++) {
    vec_set(e, i + 2, vec_get(vals, i));
  }
  return e;
}

obj env_lookup(vector* env, symbol* name) {
  while (env != NULL) {
    vector* names = (vector*)vec_get(env, 1);
    int count = vec_count(names);
    for (int i = 0; i < count; i++) {
      if (sym_cmp((symbol*)vec_get(names, i), name) == 0) {
        return vec_get(env, i + 2);
      }
    }
    env = (vector*)vec_get(env, 0);
  }
  return (obj)NULL;
}

// ======= reading and printing

number* read_number(mpc_ast_t* n) {
  errno = 0;
  long x = strtol(n->contents, NULL, 10);
  // ignore for now
  return make_num(x);
}

obj read_obj(mpc_ast_t* s) {
  if (strstr(s->tag, "number")) {
    return (obj)read_number(s);
  }
  else if (strstr(s->tag, "symbol")) {
    return (obj)make_sym(s->contents);
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
    vector* res = make_vec(count);
    KEEP(res);
    for (int i = 0; i < s->children_num; i++) {
      obj child = read_obj(s->children[i]);
      if (child) {
        vec_set(res, childi++, child);
      }
    }
    FORGET(1);
    return (obj)res;
  }
  else if (strstr(s->tag, "sexp")) {
    cons* list = (cons*)make_nil();
    KEEP(list);
    for (int i = s->children_num - 1; i >= 0; i--) {
      obj child = read_obj(s->children[i]);
      if (child) {
        list = make_cons(child, (obj)list);
      }
    }
    FORGET(1);
    return (obj)list;
  }
  return (obj)NULL;
}

void print_obj(obj v) {
  switch (tag(v)) {
  case LVAL_FWD:
    printf("<fwd %ld>", (long)v);
  case LVAL_NUM:
    printf("%li", *(number*)v);
    break;
  case LVAL_SYM:
    printf("%s", (char*)v);
    break;
  case LVAL_ERR:
    printf("<error %s>", (char*)v);
    break;
  case LVAL_FUNC:
  case LVAL_PRIM:
    printf("<function>");
    break;
  case LVAL_VEC:
    putchar('[');
    for (int i = 0; i < vec_count((vector*)v); i++) {
      if (i > 0) putchar(' ');
      print_obj(vec_get((vector*)v, i));
    }
    putchar(']');
    break;
  case LVAL_NIL:
    printf("()");
    break;
  case LVAL_CONS:
    {
      putchar('(');
      int space = 0;
      while (tag(v) == LVAL_CONS) {
        if (space) putchar(' ');
        space = 1;
        print_obj(car((cons*)v));
        v = cdr((cons*)v);
      }
      if (tag(v) != LVAL_NIL) {
        puts(".");
        print_obj(v);
      }
      putchar(')');
      break;
    }
  }
}

void print_env(vector* env) {
  while (env != NULL) {
    vector* names = (vector*)vec_get(env, 1);
    puts("--env--");
    for (int i = 0; i < vec_count(names); i++) {
      print_obj(vec_get(names, i)); puts(" = ");
      print_obj(vec_get(env, i + 2));
      putchar('\n');
    }
    env = (vector*)vec_get(env, 0);
  }
}

// === eeeeeeval

obj eval_cons(vector*, cons*);
obj eval_vec(vector*, vector*);

obj eval(vector* env, obj e) {
  switch (tag(e)) {
  case LVAL_NUM:
    return e;
  case LVAL_SYM:
    {
      obj v = env_lookup(env, (symbol*)e);
      if (v == (obj)NULL) return (obj)make_err("Undefined variable");
      else return v;
    }
  case LVAL_VEC:
    return eval_vec(env, (vector*)e);
  case LVAL_NIL:
    return e;
  case LVAL_CONS:
    return eval_cons(env, (cons*)e);
  default:
    return (obj)make_err("I can't eval this");
  }
}

obj eval_vec(vector* env, vector* vec) {
  KEEP(vec);
  KEEP(env);
  vector* res = make_vec(vec_count(vec));
  KEEP(res);
  for (int i = 0; i < vec_count(vec); i++) {
    vec_set(res, i, eval(env, vec_get(vec, i)));
  }
  FORGET(3);
  return (obj)res;
}

// assumes a list of expressions, anything else is bad syntax
obj eval_progn(vector* env, cons* exprs) {
  obj val = (obj)NULL;
  KEEP(env); KEEP(exprs); KEEP(val);
  while (tag(exprs) == LVAL_CONS) {
    val = eval(env, car(exprs));
    exprs = (cons*)cdr(exprs);
  }
  FORGET(3);
  return val;
}

// Calling convention: stick 'em in a vector. This means we have to
// count them first, but oh well. When there's a stack, they can go
// there.
obj eval_application(vector* env, obj head, cons* exprs) {
  obj res = (obj)NULL;
  KEEP(env); KEEP(head); KEEP(exprs);
  int count = cons_len(exprs);
  vector* argv = make_vec(count);
  KEEP(argv);
  int i = 0;
  while (tag(exprs) == LVAL_CONS) {
    vec_set(argv, i++, eval(env, car(exprs)));
    exprs = (cons*)cdr(exprs);
  }

  if (tag(head) == LVAL_PRIM) {
    prim_fun f = *(prim*)head;
    // arity??
    res = f(count, argv);
  }
  else if (tag(head) == LVAL_FUNC) {
    closure* f = (closure*)head;
    if (vec_count(f->formals) != count) {
      res = (obj)make_err("Wrong arity");
    }
    else {
      vector* newenv = env_extend(f->env, f->formals, argv);
      res = eval_progn(newenv, f->body);
    }
  }
  else {
    res = (obj)make_err("Not a function in the head of the expression");
  }
  FORGET(4);
  return res;
}

obj eval_cons(vector* env, cons* s) {
  obj head = car(s);
  KEEP(env); KEEP(s); KEEP(head);
  // special forms
  obj res;

  if (tag(head) == LVAL_SYM) {
    if (sym_cmp((symbol*)head, (symbol*)"lambda") == 0) {
      vector* args = (vector*)cadr(s);
      if (tag(args) != LVAL_VEC) {
        //puts("args not a vector "); print_lval(args);
        res = (obj)make_err("Args in lambda must be a vector");
        goto end;
      }
      cons* body = (cons*)cddr(s);
      if (tag(body) != LVAL_CONS) {
        res = (obj)make_err("Need expressions for body of lambda");
        goto end;
      }

      // check we have all symbols
      for (int i = 0; i < vec_count(args); i++) {
        if (tag(vec_get(args, i)) != LVAL_SYM) {
          //puts("formal not a symbol "); print_lval(lvec_get(args, i));
          
          res = (obj)make_err("Formals of lambda must be symbols");
          goto end;
        }
      }
      closure* c = make_clos(args, env, body);
      res = (obj)c;
      goto end;
    }
  }
  // ok treat it like a function
  head = eval(env, head);
  res = eval_application(env, head, (cons*)cdr(s));
 end:
  FORGET(3);
  return res;
}

obj prim_plus(int argc, vector* argv) {
  long result = 0;
  for (int i = 0; i < argc; i++) {
    obj arg = vec_get(argv, i);
    if (tag(arg) != LVAL_NUM) {
      return (obj)make_err("Arguments to + must be numbers");
    }
    result += *(number*)arg;
  }
  return (obj)make_num(result);
}

obj prim_list(int argc, vector* argv) {
  if (argc == 0) {
    return (obj)make_nil();
  }
  else {
    KEEP(argv);
    obj res = (obj)make_nil();
    for (int i = vec_count(argv) - 1; i >= 0; i--) {
      res = (obj)make_cons(vec_get(argv, i), res);
    }
    FORGET(1);
    return res;
  }
}

vector* init_toplevel() {
  int c = 2;

  vector* names = make_vec(c);
  vector* prims = make_vec(c);

  vec_set(names, 0, (obj)make_sym("+"));
  vec_set(prims, 0, (obj)make_prim(&prim_plus));

  vec_set(names, 1, (obj)make_sym("list"));
  vec_set(prims, 1, (obj)make_prim(&prim_list));
  
  // assume we won't do a collection here
  return env_extend(NULL, names, prims) ;
}

void eval_root(vector* toplevel, mpc_ast_t* root) {
  int multi = 0;
  for (int i = 0; i < root->children_num; i++) {
    obj inval = read_obj(root->children[i]);
    if (inval) {
      if (multi) putchar('\n');
      //printf(";; "); print_obj(inval); puts(" ->");
      obj res = eval(toplevel, inval);
      print_obj(res);
      multi = 1;
    }
  }
}

// We can get either an expression, or program
void print_root(mpc_ast_t* root) {
  int multi = 0;
  for (int i = 0; i < root->children_num; i++) {
    obj inval = read_obj(root->children[i]);
    if (inval) {
      if (multi) putchar(' ');
      print_obj(inval);
      multi = 1;
    }
  }
}

int main(int argc, char** argv) {
  static char* prompt = "lispy> ";

  // don't buffer stdout
  setbuf(stdout, NULL);

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

  gc_init();
  vector* toplevel = init_toplevel();
  gc_push_root((obj*)&toplevel);

  if (argc > 1) {
    mpc_result_t r;
    for (int i = 1; i < argc; i++) {
      if (mpc_parse("<commandline>", argv[i], Program, &r)) {
        mpc_ast_t* root = r.output;
        eval_root(toplevel, root);
        putchar('\n');
        //mpc_ast_print(r.output);
        mpc_ast_delete(r.output);
      }
      else {
        mpc_err_print(r.error);
        mpc_err_delete(r.error);
      }
    }
    return 0;
  }
  // else

  puts("Lispy version 0.0.3");
  puts("Press Ctrl+C to exit");

  while (1) {
    char* in = readline(prompt);
    mpc_result_t r;
    if (mpc_parse("<stdin>", in, Program, &r)) {
      add_history(in);
      mpc_ast_t* root = r.output;
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
