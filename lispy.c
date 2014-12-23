#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include <editline/readline.h>

#include "mpc/mpc.h"

#ifdef DEBUG
#define debug(x) { x; }
#else
#define debug(x)
#endif

// ==== values

enum lval_tag { LVAL_FWD, LVAL_NUM, LVAL_SYM, LVAL_CONS,
                LVAL_NIL, LVAL_VEC, LVAL_FUNC, LVAL_PRIM,
                LVAL_STR, LVAL_ERR };

typedef intptr_t obj;

typedef struct {
  uint64_t tag:8;
  uint64_t size:56;
} header;

typedef long number;
typedef char symbol;
typedef char string;
typedef char err;
typedef struct {
  int count;
} vector;
typedef obj (*prim_fun)(vector*);
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

// Protect ourselves from the representation of symbols
static inline char* sym_name(symbol* s) {
  return (char*)s;
}

static inline int sym_cmp(symbol* a, symbol* b) {
  return strcmp(sym_name(a), sym_name(b));
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

static inline int lval_truthy(obj v) {
  return !(tag(v) == LVAL_NIL);
}

// garbage collection

#define STACK_SIZE 2048
#define HEAP_SIZE 2048

obj** root_stack;
int root_stack_ptr = 0;

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
  root_stack = malloc(sizeof(obj*) * STACK_SIZE);
  fromspace = malloc(HEAP_SIZE);
  tospace = malloc(HEAP_SIZE);
  gc_flip();
}

void gc_push_root(obj* root) {
  root_stack[root_stack_ptr++] = root;
  assert(root_stack_ptr < STACK_SIZE);
}

void gc_pop_roots(int entries) {
  root_stack_ptr -= entries;
  assert(root_stack_ptr >= 0);
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
  for (int i = 0; i < root_stack_ptr; i++) {
    *root_stack[i] = gc_copy(*root_stack[i]);
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

string* make_str(char* characters) {
  int size = strlen(characters) + 1;
  string* result = (string*)alloc_obj(LVAL_STR, size);
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
  else if (strstr(s->tag, "string")) {
    return (obj)make_str(s->contents);
  }
  else if (strstr(s->tag, "vector")) {
    // gross
    int count = 0;
    for (int i = 0; i < s->children_num; i++) {
      mpc_ast_t* s1 = s->children[i];
      if (strstr(s1->tag, "number") ||
          strstr(s1->tag, "symbol") ||
          strstr(s1->tag, "string") ||
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
  if (v == (obj)NULL) { printf("<undefined>"); return; }
  switch (tag(v)) {
  case LVAL_FWD:
    printf("<fwd %ld>", (long)v);
  case LVAL_NUM:
    printf("%li", *(number*)v);
    break;
  case LVAL_SYM:
    printf("%s", sym_name((symbol*)v));
    break;
  case LVAL_STR:
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
      print_obj(vec_get(names, i)); printf(" = ");
      print_obj(vec_get(env, i + 2));
      putchar('\n');
    }
    env = (vector*)vec_get(env, 0);
  }
}

// === eeeeeeval

enum frame_type { FRAME_EMPTY, FRAME_APPLY, FRAME_IF, FRAME_PROGN };

typedef struct {
  enum frame_type type;
  vector* env;
  vector* args;
  int fill;
} frame;

static inline char* frame_type(enum frame_type t) {
  switch (t) {
  case FRAME_EMPTY: return "empty";
  case FRAME_APPLY: return "funcall";
  case FRAME_IF: return "if";
  case FRAME_PROGN: return "do";
  }
}

void print_frame(frame* f) {
  printf("--frame (%s)--\n", frame_type(f->type));
  puts("---args---");
  print_obj((obj)f->args); putchar('\n');
  print_env(f->env);
  puts("-----");
}

frame* stack;
int stackptr;

int eval_special(cons* expr, obj* valreg, vector** envreg) {
  KEEP(expr);
  int result = 0;
  obj head = car(expr);
  if (tag(head) == LVAL_SYM) {
    if (strcmp(sym_name((symbol*)head), "lambda") == 0) {
      vector* args = (vector*)cadr(expr);
      cons* body = (cons*)car((cons*)cddr(expr));
      *valreg = (obj)make_clos(args, *envreg, body);
      result = 1;
    }
    else if (strcmp(sym_name((symbol*)head), "if") == 0) {
      vector* ks = make_vec(2);
      vec_set(ks, 0, car((cons*)cddr(expr)));
      vec_set(ks, 1, cadr((cons*)cddr(expr)));
      *valreg = cadr(expr);
      frame* f = &stack[++stackptr];
      f->type = FRAME_IF;
      f->args = ks;
      result = 1;
    }
    else if (strcmp(sym_name((symbol*)head), "do") == 0) {
      int len = cons_len(expr) - 1;
      *valreg = cadr(expr);
      if (len > 1) {
        vector* exprs = make_vec(len - 1);
        frame* f = &stack[++stackptr];
        f->type = FRAME_PROGN;
        f->fill = 0;
        expr = (cons*)cddr(expr);
        while (tag(expr) == LVAL_CONS) {
          vec_set(exprs, f->fill++, car((cons*)expr));
          expr = (cons*)cdr((cons*)expr);
        }
        f->args = exprs;
        f->fill = 0;
      }
      result = 1;
    }
    else result = 0;
  }
  FORGET(1);
  return result;
}

obj eval_loop(vector* toplevel, obj expr) {

  stack = malloc(sizeof(frame) * STACK_SIZE);
  stackptr = 0;
  frame* mt = &stack[0];
  mt->type = FRAME_EMPTY;

  obj val = expr;
  vector* env = toplevel;
  KEEP(val); KEEP(env);

 eval:
  switch (tag(val)) {
  case LVAL_CONS:
    {
      if (!eval_special((cons*)val, &val, &env)) {
        int len = cons_len((cons*)val);
        frame* f = &stack[++stackptr];
        f->type = FRAME_APPLY;
        f->env = env;
        f->args = make_vec(len + 1);
        vec_set(f->args, 0, cdr((cons*)val));
        f->fill = 1;
        val = car((cons*)val);
        debug(printf("pushed frame size %d\n", len));
        debug(print_frame(f));
      }
      goto eval;
    }
  case LVAL_SYM:
    debug(printf("lookup %s", sym_name((symbol*)val)));
    val = env_lookup(env, (symbol*)val);
    debug(printf(", found ")); debug(print_obj(val)); debug(puts(""));
    goto apply_k;
  default:
    goto apply_k;
  }
  
 apply_k:
  assert(stackptr >= 0);
  frame* f = &stack[stackptr];
  debug(printf("stack frame at %d\n", stackptr));
  debug(print_frame(f));
  switch (f->type) {
  case FRAME_APPLY: {
    vec_set(f->args, f->fill++, val);
    debug(printf("set slot %d to ", f->fill - 1));
    debug(print_obj(val)); debug(putchar('\n'));
    cons* todo = (cons*)vec_get(f->args, 0);
    if (tag(todo) == LVAL_NIL) {
      stackptr--; // now or later, for GC?
      obj head = vec_get(f->args, 1);
      if (tag(head) == LVAL_FUNC) {
        closure* func = (closure*)head;
        vec_set(f->args, 1, (obj)func->formals);
        vec_set(f->args, 0, (obj)f->env);
        env = f->args;
        val = (obj)func->body;
      }
      else if (tag(head) == LVAL_PRIM) {
        prim_fun func = *(prim*)head;
        // don't care about formals or parent env
        val = func(f->args);
      }
      goto apply_k;
    }
    else {
      vec_set(f->args, 0, cdr(todo));
      val = car(todo);
      goto eval;
    }
  }

  case FRAME_IF:
    if (lval_truthy(val)) {
      val = vec_get(f->args, 0);
    }
    else {
      val = vec_get(f->args, 1);
    }
    stackptr--;
    goto eval;

  case FRAME_PROGN:
    if (f->fill < vec_count(f->args) - 1) {
      val = vec_get(f->args, f->fill++);
    }
    else {
      val = vec_get(f->args, f->fill++);
      stackptr--;
    }
    goto eval;

  case FRAME_EMPTY:
    FORGET(2);
    return val;
  }
}

obj prim_plus(vector* argv) {
  long result = 0;
  for (int i = 2; i < vec_count(argv); i++) {
    obj arg = vec_get(argv, i);
    if (tag(arg) != LVAL_NUM) {
      return (obj)make_err("Arguments to + must be numbers");
    }
    result += *(number*)arg;
  }
  return (obj)make_num(result);
}

obj prim_list(vector* argv) {
  KEEP(argv);
  obj res = (obj)make_nil();
  KEEP(res);
  for (int i = vec_count(argv) - 1; i >= 2; i--) {
    res = (obj)make_cons(vec_get(argv, i), res);
  }
  FORGET(2);
  return res;
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
      obj res = eval_loop(toplevel, inval);
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
  mpc_parser_t* String = mpc_new("string");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Sexp = mpc_new("sexp");
  mpc_parser_t* Vector = mpc_new("vector");
  mpc_parser_t* Program = mpc_new("program");
  
  mpca_lang(MPCA_LANG_DEFAULT, "                                   \
    number   : /-?[0-9]+/ ;                                        \
    symbol   : /[-a-zA-Z_0-9?!+*\\/]+/ ;                           \
    string   : /\"(\\\\.|[^\"])*\"/ ;                              \
    expr     : <number> | <symbol> | <string> | <sexp> | <vector> ;\
    sexp     : '(' <expr>* ')' ;                                   \
    vector   : '[' <expr>* ']' ;                                   \
    program  : /^/ <expr>* /$/ ;                                   \
  ", Number, Symbol, String, Expr, Sexp, Vector, Program);

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

  mpc_cleanup(7, Number, Symbol, String, Expr, Sexp, Vector, Program);

  return 0;
}
