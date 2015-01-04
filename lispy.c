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

char* lval_tag_name(enum lval_tag t) {
  switch (t) {
  case LVAL_FWD: return "fwd pointer";
  case LVAL_NUM: return "number";
  case LVAL_SYM: return "symbol";
  case LVAL_CONS: return "cons";
  case LVAL_NIL: return "nil";
  case LVAL_VEC: return "vector";
  case LVAL_FUNC: return "function";
  case LVAL_PRIM: return "primitive";
  case LVAL_STR: return "string";
  case LVAL_ERR: return "error";
  }
}

#define UNDEFINED ((obj)NULL)

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
  return !(tag(v) == LVAL_SYM &&
           strcmp(sym_name((symbol*)v), "false") == 0);
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

#define KEEP(var) do {                                          \
    debug(printf("Pushing " #var " onto root stack\n"));        \
    gc_push_root((obj*)&(var));                                 \
  } while (0)
#define FORGET(num) do {                        \
    debug(printf("Popping %d roots\n", num));   \
    gc_pop_roots(num);                          \
  } while (0)

void print_obj(obj);

#define tospace_offset(ptr) ((long)ptr - (long)tospace)
#define fromspace_offset(ptr) ((long)ptr - (long)fromspace)

obj gc_copy(obj o) {
  debug(printf("GC copy object: ")); debug(print_obj(o)); debug(puts(""));
  if (o == UNDEFINED) return UNDEFINED;
  else if (tag(o) == LVAL_FWD) {
    fwd* f = (fwd*)(o - sizeof(header));
    debug(printf("Found fwd pointer at to:%ld pointing to from:%ld\n",
                 tospace_offset(f), fromspace_offset(f->obj)));
    assert((intptr_t)f->obj < (intptr_t)next &&
           (intptr_t)f->obj >= (intptr_t)fromspace);
    return f->obj;
  }
  else {
    fwd* from = (fwd*)((header*)o - 1);
    obj to = (obj)(next + sizeof(header));
    int bytes = sizeof(header) + from->hdr.size * sizeof(intptr_t);
    memmove((void*)next, (void*)from, bytes);
    debug(printf("Copied obj at to:%ld size %d to from:%ld\n",
                 tospace_offset(from), bytes, fromspace_offset(next)));
    from->hdr.tag = LVAL_FWD;
    from->hdr.size = 1;
    from->obj = to;
    debug(printf("Left fwd pointer at to:%ld to from:%ld\n",
                 tospace_offset(from), fromspace_offset(from->obj)));
    debug(print_obj((obj)from + 8)); debug(putchar('\n'));
    next += bytes;
    return to;
  }
}

void gc_copy_frame_stack();

void gc_dump_space() {
  header* at = (header*)fromspace;
  printf("Dumping fromspace at %ld, extent %ld (size %ld)\n",
         (long)fromspace, (long)next, (long)next - (long)fromspace);
  while ((intptr_t)at < next) {
    printf("Object at from:%ld tag %d size %ld:\n", fromspace_offset(at),
           (int)at->tag, (long)at->size);
    print_obj((obj)(at + 1)); putchar('\n');
    at += (1 + at->size);
  }
}

void gc() {
#ifdef DEBUG
  long oldnext = next;
#endif
  gc_flip();
  debug(printf("Moving objects from space at %ld to space at %ld\n",
               (long)tospace, (long)fromspace));
  // phase one: copy over everything on the root stack and frame stack
  debug(printf("Root stack is %d deep\n", root_stack_ptr));
  for (int i = 0; i < root_stack_ptr; i++) {
    debug(printf("Copying root #%d\n", i));
    *root_stack[i] = gc_copy(*root_stack[i]);
  }
  gc_copy_frame_stack();

  // phase two: copy over things reachable from anything we copied earlier
  intptr_t todo = (intptr_t)fromspace;
  while (todo < next) {
    debug(printf("Examining object at from:%ld\n", fromspace_offset(todo)));
    header* h = (header*)todo;
    enum lval_tag tag = h->tag;
    debug(printf("It's a %s, size %d\n", lval_tag_name(tag), (int)h->size));
    assert(tag != LVAL_FWD);
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
    int bytes = sizeof(header) + h->size * sizeof(intptr_t);
    debug(printf("Skipping over %d bytes\n", bytes));
    todo += bytes;
  }
  debug(printf("GC done. Freed %ld bytes\n",
               ((intptr_t)oldnext - (intptr_t)tospace) -
               ((intptr_t)next - (intptr_t)fromspace)));
  debug(printf("==== New heap ====\n"));
  debug(gc_dump_space());
  debug(printf("==================\n"));
}

static inline obj alloc_obj(enum lval_tag tag, int size) {
  // sizeof(header) will be padded anyway, so the sum will be rounded
  assert(tag != LVAL_FWD);
  assert(size > 0);
  int bytes = sizeof(header) + round_to_word(size);
  debug(printf("Allocating %d bytes with %ld bytes allocated\n", bytes, (next - (long)fromspace)));
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
  memset((void*)result, 0, size);
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
  return UNDEFINED;
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
  return UNDEFINED;
}

void print_obj(obj v) {
  if (v == UNDEFINED) { printf("<undefined>"); return; }
  switch (tag(v)) {
  case LVAL_FWD:
    printf("<fwd %ld>", (long)v);
    break;
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
  putchar('(');
  while (env != NULL) {
    vector* names = (vector*)vec_get(env, 1);
    putchar('{');
    for (int i = 0; i < vec_count(names); i++) {
      if (i > 0) putchar(',');
      print_obj(vec_get(names, i)); printf(" = ");
      print_obj(vec_get(env, i + 2));
    }
    putchar('}');
    env = (vector*)vec_get(env, 0);
    if (env != NULL) putchar(',');
  }
  putchar(')');
}

// === eeeeeeval

enum frame_type { FRAME_EMPTY, FRAME_APPLY, FRAME_IF, FRAME_PROGN, FRAME_VEC };

typedef struct frame {
  enum frame_type type;
  vector* env;
  obj args;
  int fill;
} frame;

static inline char* frame_type(enum frame_type t) {
  switch (t) {
  case FRAME_EMPTY: return "empty";
  case FRAME_APPLY: return "funcall";
  case FRAME_IF: return "if";
  case FRAME_PROGN: return "do";
  case FRAME_VEC: return "vector";
  }
}

void print_frame(frame* f) {
  printf("--frame (%s)--\n", frame_type(f->type));
  puts("---args---");
  print_obj(f->args); putchar('\n');
  print_env(f->env);
  puts("-----");
}

frame* stack;
int stackptr;

void gc_copy_frame_stack() {
  for (int i = 0; i <= stackptr; i++) {
    frame* f = &stack[stackptr];
    debug(printf("Examing frame #%d, it's a %s\n", i, frame_type(f->type)));
    f->args = gc_copy((obj)f->args);
    f->env = (vector*)gc_copy((obj)f->env);
  }
}

static inline frame* stack_push(vector* env) {
  debug(puts("push frame"));
  frame* f = &stack[++stackptr];
  f->env = env;
  return f;
}

static inline frame* stack_peek() {
  return &stack[stackptr];
}

static inline void stack_pop(vector** env) {
  debug(puts("pop frame"));
  frame* f = stack_peek();
  debug(printf("restoring env: ")); debug(print_env(f->env)); debug(putchar('\n'));
  *env = f->env;
  stackptr--;
}

int eval_special(cons* expr, obj* valreg, vector** envreg) {
  KEEP(expr);
  int result = 1;
  obj head = car(expr);
  if (tag(head) == LVAL_SYM) {
    if (strcmp(sym_name((symbol*)head), "lambda") == 0) {
      vector* args = (vector*)cadr(expr);
      cons* body = (cons*)cddr(expr);
      debug(puts("Making lambda"));
      debug(print_obj((obj)args)); debug(putchar('\n'));
      debug(print_obj((obj)body)); debug(putchar('\n'));
      *valreg = (obj)make_clos(args, *envreg, body);
    }
    else if (strcmp(sym_name((symbol*)head), "if") == 0) {
      vector* ks = make_vec(2);
      vec_set(ks, 0, car((cons*)cddr(expr)));
      vec_set(ks, 1, cadr((cons*)cddr(expr)));
      *valreg = cadr(expr);
      frame* f = stack_push(*envreg);
      f->type = FRAME_IF;
      f->args = (obj)ks;
    }
    else if (strcmp(sym_name((symbol*)head), "do") == 0) {
      int len = cons_len(expr) - 1;
      *valreg = cadr(expr);
      if (len > 1) {
        frame* f = stack_push(*envreg);
        f->type = FRAME_PROGN;
        f->args = cddr(expr);
        f->fill = 0;
      }
    }
    else result = 0;
  }
  else result = 0;
  FORGET(1);
  return result;
}

obj eval_loop(vector* toplevel, obj expr) {

  frame* f = stack_peek();
  f->type = FRAME_EMPTY;

  obj val = expr;
  vector* env = toplevel;
  KEEP(val); KEEP(env);

 eval:
  debug(printf("eval: ")); debug(print_obj(val)); debug(putchar('\n'));
  debug(printf("in env: ")); debug(print_env(env)); debug(putchar('\n'));

  switch (tag(val)) {
  case LVAL_CONS:
    {
      if (!eval_special((cons*)val, &val, &env)) {
        int len = cons_len((cons*)val);
        f = stack_push(env);
        f->type = FRAME_APPLY;
        f->args = (obj)make_vec(len + 1);
        vec_set((vector*)f->args, 0, cdr((cons*)val));
        f->fill = 1;
        val = car((cons*)val);
      }
      goto eval;
    }
  case LVAL_VEC:
    {
      if (vec_count((vector*)val) == 0) goto apply_k;
      f = stack_push(env);
      f->type = FRAME_VEC;
      f->args = val;
      f->fill = 0;
      val = vec_get((vector*)f->args, 0);
    }
    goto eval;
  case LVAL_SYM:
    val = env_lookup(env, (symbol*)val);
    goto apply_k;
  default:
    goto apply_k;
  }
  
 apply_k:
  assert(stackptr >= 0);
  f = stack_peek();

  debug(printf("apply: ")); debug(print_obj(val)); debug(putchar('\n'));
  debug(printf("with frame: ")); debug(print_frame(f));

  switch (f->type) {

  case FRAME_APPLY: {
    vec_set((vector*)f->args, f->fill++, val);
    cons* todo = (cons*)vec_get((vector*)f->args, 0);
    if (tag(todo) == LVAL_NIL) { // no more arguments to eval, enter func
      obj head = vec_get((vector*)f->args, 1);
      if (tag(head) == LVAL_FUNC) {
        closure* func = (closure*)head;
        vector* newenv = (vector*)f->args;
        vec_set(newenv, 0, (obj)func->env);
        vec_set(newenv, 1, (obj)func->formals);
        if (tag(cdr(func->body)) == LVAL_CONS) {
          val = car(func->body);
          // NB reuse stack frame
          f->type = FRAME_PROGN;
          f->args = cdr(func->body);
          f->fill = 0;
          f->env = env;
        }
        else {
          stack_pop(&env);
          val = car(func->body);
        }
        // either way we want the new env to eval what's in val
        env = newenv;
        goto eval;
      }
      else if (tag(head) == LVAL_PRIM) {
        prim_fun func = *(prim*)head;
        // don't care about formals or parent env
        stack_pop(&env);
        val = func((vector*)f->args);
        goto apply_k;
      }
    }
    else {
      vec_set((vector*)f->args, 0, cdr(todo));
      val = car(todo);
      goto eval;
    }
  }

  case FRAME_VEC:
    vec_set((vector*)f->args, f->fill++, val);
    if (f->fill < vec_count((vector*)f->args)) {
      val = vec_get((vector*)f->args, f->fill);
      goto eval;
    }
    else {
      stack_pop(&env);
      val = f->args;
      goto apply_k;
    }

  case FRAME_IF:
    if (lval_truthy(val)) {
      val = vec_get((vector*)f->args, 0);
    }
    else {
      val = vec_get((vector*)f->args, 1);
    }
    stack_pop(&env);
    goto eval;

  case FRAME_PROGN:
    if (tag(f->args) == LVAL_NIL) {
      stack_pop(&env);
      goto apply_k;
    }
    else {
      val = car((cons*)f->args);
      f->args = cdr((cons*)f->args);
    }
    goto eval;

  case FRAME_EMPTY:
    FORGET(2);
    return val;
  }
}

#define NUM_OP(cname, op, zero, sname)                                  \
  obj cname(vector* argv) {                                             \
    if (vec_count(argv) < 3) {                                          \
      return (obj)make_num(zero);                                       \
    }                                                                   \
    obj arg = vec_get(argv, 2);                                         \
    if (! tag(arg) == LVAL_NUM) {                                       \
      return (obj)make_err("Arguments to " #sname " must be numbers");  \
    }                                                                   \
    long result = *(number*)arg;                                        \
    for (int i = 3; i < vec_count(argv); i++) {                         \
      arg = vec_get(argv, i);                                           \
      if (tag(arg) != LVAL_NUM) {                                       \
        return (obj)make_err("Arguments to " #sname " must be numbers"); \
      }                                                                 \
      result = result op *(number*)arg;                                 \
    }                                                                   \
    return (obj)make_num(result);                                       \
  }                                                                     \

NUM_OP(prim_plus, +, 0, +);
NUM_OP(prim_minus, -, 0, -);
NUM_OP(prim_mult, *, 1, *);
NUM_OP(prim_div, /, 1, /);

#define NUM_COMP(cname, comp, sname) obj cname(vector* argv) {          \
    if (vec_count(argv) < 3) {                                          \
      return (obj)make_sym("true");                                     \
    }                                                                   \
    obj arg = vec_get(argv, 2);                                         \
    if (tag(arg) != LVAL_NUM) {                                         \
      return (obj)make_err("Arguments to " #sname " must be numbers");  \
    }                                                                   \
    long a = *(number*)arg;                                             \
    for (int i = 3; i < vec_count(argv); i++) {                         \
      arg = vec_get(argv, i);                                           \
      if (tag(arg) != LVAL_NUM) {                                       \
        return (obj)make_err("Arguments to " #sname " must be numbers"); \
      }                                                                 \
      long b = *(number*)arg;                                           \
      if (!(a comp b)) {                                                \
        return (obj)make_sym("false");                                  \
      }                                                                 \
      a = b;                                                            \
    }                                                                   \
    return (obj)make_sym("true");                                       \
  }

NUM_COMP(prim_numeq, ==, =);
NUM_COMP(prim_lt, <, <);
NUM_COMP(prim_lte, <=, <=);
NUM_COMP(prim_gt, >, >);
NUM_COMP(prim_gte, >=, >=);

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
  int c = 10;
  // assume we won't do a collection here
  vector* names = make_vec(c);
  vector* prims = make_vec(c);

  vec_set(names, 0, (obj)make_sym("+"));
  vec_set(prims, 0, (obj)make_prim(&prim_plus));
  vec_set(names, 1, (obj)make_sym("-"));
  vec_set(prims, 1, (obj)make_prim(&prim_minus));
  vec_set(names, 2, (obj)make_sym("*"));
  vec_set(prims, 2, (obj)make_prim(&prim_mult));
  vec_set(names, 3, (obj)make_sym("/"));
  vec_set(prims, 3, (obj)make_prim(&prim_div));
  vec_set(names, 4, (obj)make_sym("<"));
  vec_set(prims, 4, (obj)make_prim(&prim_lt));
  vec_set(names, 5, (obj)make_sym("<="));
  vec_set(prims, 5, (obj)make_prim(&prim_lte));
  vec_set(names, 6, (obj)make_sym(">"));
  vec_set(prims, 6, (obj)make_prim(&prim_gt));
  vec_set(names, 7, (obj)make_sym(">="));
  vec_set(prims, 7, (obj)make_prim(&prim_gte));
  vec_set(names, 8, (obj)make_sym("="));
  vec_set(prims, 8, (obj)make_prim(&prim_numeq));
  vec_set(names, 9, (obj)make_sym("list"));
  vec_set(prims, 9, (obj)make_prim(&prim_list));
  
  return env_extend(NULL, names, prims) ;
}

void eval_root(vector* toplevel, mpc_ast_t* root) {
  KEEP(toplevel);
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
  FORGET(1);
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
    symbol   : /[-<>=+*\\/a-zA-Z_0-9!?]+/ ;                        \
    string   : /\"(\\\\.|[^\"])*\"/ ;                              \
    expr     : <number> | <symbol> | <string> | <sexp> | <vector> ;\
    sexp     : '(' <expr>* ')' ;                                   \
    vector   : '[' <expr>* ']' ;                                   \
    program  : /^/ <expr>* /$/ ;                                   \
  ", Number, Symbol, String, Expr, Sexp, Vector, Program);

  gc_init();
  stack = malloc(sizeof(frame) * STACK_SIZE);
  stackptr = 0;

  vector* toplevel;
  KEEP(toplevel);
  toplevel = init_toplevel();

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

  FORGET(1);
  mpc_cleanup(7, Number, Symbol, String, Expr, Sexp, Vector, Program);

  return 0;
}
