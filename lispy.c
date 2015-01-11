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

enum lval_type { LVAL_FWD = 0,
                 LVAL_NUM = 1,
                 LVAL_SYM = 2,
                 LVAL_CONS = 3,
                 LVAL_NIL = 4,
                 LVAL_VEC = 5,
                 LVAL_FUNC = 6,
                 LVAL_PRIM = 7,
                 // don't look like a fwd
                 LVAL_STR = 9,
                 LVAL_ERR = 10,
                 LVAL_IMM_SYM = 11 };

char* lval_type_name(enum lval_type t) {
  switch (t) {
  case LVAL_FWD: return "fwd pointer";
  case LVAL_NUM: return "number";
  case LVAL_IMM_SYM: // fall through
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

#define TAG_MASK 0xffffffffffffff00
#define PTR_MASK 0xfffffffffffffff8
// 1100
#define TAG_IMM_SYM 12
// 010
#define TAG_PRIM 2

typedef intptr_t obj;

typedef struct {
  uint64_t type:8;
  uint64_t size:56;
} header;

typedef intptr_t number;
typedef intptr_t symbol;
typedef char string;
typedef char err;
typedef struct {
  int count;
} vector;
typedef obj (*prim_fun)(vector*);
struct prim_s {
  char* name;
  prim_fun func;
};
typedef intptr_t prim;
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

static inline enum lval_type obj_type(obj val) {
  // integer: the lowest bit is 1
  if (((intptr_t)val & 1) == 1) return LVAL_NUM;
  // pointer to primitive struct: last three bits are 010
  if (((intptr_t)val & 7) == TAG_PRIM) return LVAL_PRIM;
  // inline symbol: the last byte is 0000 1100
  if (((intptr_t)val & 255) == TAG_IMM_SYM) return LVAL_IMM_SYM;
  // Otherwise look at the header
  header* h = (header*)val - 1;
  if ((h->type & 7) == 0) return LVAL_FWD;
  else return ((header*)h)->type;
}
#define type(v) (obj_type((obj)(v)))

static inline int lval_is_immediate(obj o) {
  return ! (((intptr_t)o & 7) == 0);
}

static inline int lval_is_sym(obj o) {
  return type(o) == LVAL_IMM_SYM || type(o) == LVAL_IMM_SYM;
}

static inline obj fwd_target(obj o) {
  return *((obj*)o - 1);
}

static inline number num_value(obj o) {
  return (number)o >> 1;
}

static inline struct prim_s* prim_value(obj o) {
  return (struct prim_s*)(o & PTR_MASK);
}

// Protect ourselves from the representation of symbols
#define sym_name(s) ((type(s) == LVAL_IMM_SYM) ? \
                     ((char*)(&(s)) + 1) : (char*)(s))

static inline int sym_cmp(symbol a, symbol b) {
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
  return !(lval_is_sym(v) && strcmp(sym_name(v), "false") == 0);
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

#define KEEP(var) do {                                                  \
    debug(printf("Pushing " #var "@%ld onto root stack", (intptr_t)var)); \
    debug(printf(" at #%d\n", root_stack_ptr));                         \
    debug(print_obj((obj)(var))); debug(puts(""));                      \
    gc_push_root((obj*)&(var));                                         \
  } while (0)
#define FORGET(num) do {                        \
    debug(printf("Popping %d roots\n", num));   \
    gc_pop_roots(num);                          \
  } while (0)

void print_obj(obj);

#define tospace_offset(ptr) ((long)ptr - (long)tospace)
#define fromspace_offset(ptr) ((long)ptr - (long)fromspace)

obj gc_copy(obj o) {
  debug(printf("GC copy %s object: @%ld: ", lval_type_name(type(o)), o));
  debug(print_obj(o)); debug(puts(""));
  if (o == UNDEFINED) return UNDEFINED;
  else if (lval_is_immediate(o)) return o;
  else if (type(o) == LVAL_FWD) {
    obj t = fwd_target(o);
    debug(printf("Found fwd pointer at to:%ld pointing to from:%ld\n",
                 tospace_offset(o), fromspace_offset(t)));
    assert((intptr_t)t < (intptr_t)next &&
           (intptr_t)t >= (intptr_t)fromspace);
    return t;
  }
  else {
    header* h = ((header*)o - 1);
    obj to = (obj)(next + sizeof(header));
    assert(((intptr_t)to & 7) == 0); // bottom bits are zero
    int bytes = sizeof(header) + h->size * sizeof(intptr_t);
    memmove((void*)next, (void*)h, bytes);
    debug(printf("Copied obj at to:%ld size %d to from:%ld\n",
                 tospace_offset(h), bytes, fromspace_offset(next)));
    *(obj*)h = to;
    debug(printf("Left fwd pointer at to:%ld to from:%ld\n",
                 tospace_offset(h), fromspace_offset(to)));
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
    printf("Object at from:%ld type %s size %ld words:\n", fromspace_offset(at),
           lval_type_name(at->type), (long)at->size);
    print_obj((obj)(at + 1)); putchar('\n');
    at += (1 + at->size);
  }
}

void gc() {
#ifdef DEBUG
  long oldnext = next;
  gc_dump_space();
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
    enum lval_type type = h->type;
    assert(type != LVAL_FWD);
    debug(printf("It's a %s, size %d\n", lval_type_name(type), (int)h->size));
    switch (type) {
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

static inline obj alloc_obj(enum lval_type type, int size) {
  // sizeof(header) will be padded anyway, so the sum will be rounded
  assert(type != LVAL_FWD);
  assert(size > 0);
  int bytes = sizeof(header) + round_to_word(size);
  debug(printf("Allocating %d bytes @%ld with %ld bytes allocated\n",
               bytes, next, (next - (long)fromspace)));
  if (next + bytes > limit) {
    gc();
    return alloc_obj(type, size);
  }
  header* h = (header*)next;
  h->type = type;
  // NB size does not include header
  h->size = round_to_word(size) / sizeof(intptr_t);
  next += bytes;
  return (obj)(h + 1);
}

// === constructing values

number make_num(long value) {
  return ((intptr_t)value << 1) | 1;
}

symbol make_sym(char* characters) {
  int size = strlen(characters) + 1;
  if (size <= sizeof(intptr_t) - 1) {
    // encode the symbol in a char array
    return (symbol) ((*((intptr_t*)(characters - 1)) & TAG_MASK) | TAG_IMM_SYM);
  }
  else {
    symbol result = (symbol)alloc_obj(LVAL_SYM, size);
    strcpy((char*)result, characters);
    return result;
  }
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

prim make_prim(struct prim_s* p) {
  return (intptr_t)p | TAG_PRIM;
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
  if (type(s) != LVAL_CONS) {
    return (obj)make_err("Tried to get car of non-list");
  }
  return s->car;
}

static inline obj cdr(cons* s) {
  if (type(s) != LVAL_CONS) {
    return (obj)make_err("Tried to get cdr of non-list");
  }
  else {
    return s->cdr;
  }
}

static inline obj cddr(cons* s) {
  cons* cdrval = (cons*)cdr(s);
  if (type(cdrval) != LVAL_CONS) {
    return (obj)make_err("Tried to get cddr of not long enough list");
  }
  else {
    return cdrval->cdr;
  }
}

static inline obj cadr(cons* s) {
  cons* cdrval = (cons*)cdr(s);
  if (type(cdrval) != LVAL_CONS) {
    return (obj)make_err("Tried to get cadr of not long enough list");
  }
  else {
    return cdrval->car;
  }
}

static inline int cons_len(cons* s) {
  int l = 0;
  while (type(s) == LVAL_CONS) {
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

obj env_lookup(vector* env, symbol name) {
  while (env != NULL) {
    vector* names = (vector*)vec_get(env, 1);
    int count = vec_count(names);
    for (int i = 0; i < count; i++) {
      if (sym_cmp((symbol)vec_get(names, i), name) == 0) {
        return vec_get(env, i + 2);
      }
    }
    env = (vector*)vec_get(env, 0);
  }
  return UNDEFINED;
}

// ======= reading and printing

number read_number(mpc_ast_t* n) {
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
  switch (type(v)) {
  case LVAL_FWD:
    printf("<fwd %ld>", (long)fwd_target(v));
    break;
  case LVAL_NUM:
    printf("%li", num_value(v));
    break;
  case LVAL_IMM_SYM:
  case LVAL_SYM:
    printf("%s", sym_name(v));
    break;
  case LVAL_STR:
    printf("%s", (char*)v);
    break;
  case LVAL_ERR:
    printf("<error %s>", (char*)v);
    break;
  case LVAL_FUNC:
    printf("<function>");
    break;
  case LVAL_PRIM:
    printf("<primitive %s>", prim_value(v)->name);
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
      while (type(v) == LVAL_CONS) {
        if (space) putchar(' ');
        space = 1;
        print_obj(car((cons*)v));
        v = cdr((cons*)v);
      }
      if (type(v) != LVAL_NIL) {
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

enum frame_type { FRAME_EMPTY, FRAME_ARG,
                  FRAME_APPLY, FRAME_IF,
                  FRAME_PROGN, FRAME_VEC,
                  FRAME_LETREC };

typedef struct frame {
  enum frame_type type;
  vector* env;
  obj args;
  int index;
} frame;

static inline char* frame_type(enum frame_type t) {
  switch (t) {
  case FRAME_EMPTY: return "empty";
  case FRAME_APPLY: return "funcall";
  case FRAME_IF: return "if";
  case FRAME_PROGN: return "do";
  case FRAME_VEC: return "vector";
  case FRAME_ARG: return "arg";
  case FRAME_LETREC: return "letrec";
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
    frame* f = &stack[i];
    debug(printf("Examing %s frame #%d\n", frame_type(f->type), i));
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

#define SYNTAX_APPLY 2
#define SYNTAX_EVAL  1
#define SYNTAX_NONE  0

int eval_special(cons* expr, obj* valreg, vector** envreg) {
  KEEP(expr);
  int result = SYNTAX_EVAL;
  obj head = car(expr);
  if (lval_is_sym(head)) {
    char* name = sym_name(head);
    if (strcmp(name, "quote") == 0) {
      *valreg = cadr(expr);
      result = SYNTAX_APPLY;
    }
    else if (strcmp(name, "lambda") == 0) {
      vector* args = (vector*)cadr(expr);
      cons* body = (cons*)cddr(expr);
      debug(puts("Making lambda"));
      debug(print_obj((obj)args)); debug(putchar('\n'));
      debug(print_obj((obj)body)); debug(putchar('\n'));
      *valreg = (obj)make_clos(args, *envreg, body);
      result = SYNTAX_APPLY;
    }
    else if (strcmp(name, "letrec") == 0) {
      vector* args = (vector*)cadr(expr);
      assert(type(args) == LVAL_VEC);
      int c = vec_count(args);
      assert(c % 2 == 0);
      c /= 2;
      KEEP(args);
      *valreg = (obj)make_vec(c + 2);
      vec_set((vector*)*valreg, 0, (obj)*envreg);
      vector* names = make_vec(c);
      FORGET(1);
      vec_set((vector*)*valreg, 1, (obj)names);
      for (int i = 0; i < c; i++) {
        obj s = vec_get(args, i * 2);
        assert(lval_is_sym(s));
        vec_set(names, i, s);
      }
      frame* f = stack_push(*envreg);
      f->args = cdr(expr);
      f->type = FRAME_LETREC;
      f->index = 2;
      result = SYNTAX_APPLY;
    }
    else if (strcmp(name, "if") == 0) {
      vector* ks = make_vec(2);
      vec_set(ks, 0, car((cons*)cddr(expr)));
      vec_set(ks, 1, cadr((cons*)cddr(expr)));
      *valreg = cadr(expr);
      frame* f = stack_push(*envreg);
      f->type = FRAME_IF;
      f->args = (obj)ks;
    }
    else if (strcmp(name, "do") == 0) {
      int len = cons_len(expr) - 1;
      *valreg = cadr(expr);
      if (len > 1) {
        frame* f = stack_push(*envreg);
        f->type = FRAME_PROGN;
        f->args = cddr(expr);
        f->index = 0;
      }
    }
    else result = SYNTAX_NONE;
  }
  else result = SYNTAX_NONE;
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

  switch (type(val)) {
  case LVAL_CONS:
    {
      switch (eval_special((cons*)val, &val, &env)) {
      case SYNTAX_APPLY:
        goto apply_k;
      case SYNTAX_NONE:
        {
          int len = cons_len((cons*)val);
          f = stack_push(env);
          f->type = FRAME_APPLY;
          f->args = cdr((cons*)val);
          f->index = 1;
          // and at least evaluate the head
          f = stack_push(env);
          f->type = FRAME_ARG;
          f->args = (obj)make_vec(len + 1);
          f->index = 1;
          val = car((cons*)val);
        }
      }
      goto eval;
    }
  case LVAL_VEC:
    {
      if (vec_count((vector*)val) == 0) goto apply_k;
      f = stack_push(env);
      f->type = FRAME_VEC;
      f->args = val;
      f->index = 0;
      val = vec_get((vector*)f->args, 0);
    }
    goto eval;
  case LVAL_IMM_SYM: // fall through
  case LVAL_SYM:
    val = env_lookup(env, (symbol)val);
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

  case FRAME_ARG: {
    vec_set((vector*)f->args, f->index, val);
    val = f->args;
    stack_pop(&env);
    goto apply_k;
  }

  case FRAME_APPLY: {
    vector* newenv = (vector*)val;
    cons* exprs = (cons*)f->args;
    
    if (type(exprs) == LVAL_NIL) {
      obj head = vec_get(newenv, 1);
      if (type(head) == LVAL_FUNC) {
        closure* func = (closure*)head;
        vec_set(newenv, 0, (obj)func->env);
        vec_set(newenv, 1, (obj)func->formals);
        if (type(cdr(func->body)) == LVAL_CONS) {
          val = car(func->body);
          // NB reuse stack frame
          f->type = FRAME_PROGN;
          f->args = cdr(func->body);
          f->index = 0;
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
      else if (type(head) == LVAL_PRIM) {
        prim_fun func = prim_value(head)->func;
        // don't care about formals or parent env
        stack_pop(&env);
        val = func(newenv);
        goto apply_k;
      }
    }
    else { // args left to do
      f->index++;
      int i = f->index;
      val = car((cons*)f->args);
      f->args = cdr((cons*)f->args);
      f = stack_push(env);
      f->type = FRAME_ARG;
      f->index = i;
      f->args = (obj)newenv;
      goto eval;
    }
  }

  case FRAME_LETREC: {
    vector* newenv = (vector*)val;
    vector* args = (vector*)car((cons*)f->args);
    if (f->index < vec_count(newenv)) {
      int i = f->index;
      f->index++; // next index
      f = stack_push(newenv);
      f->type = FRAME_ARG;
      f->args = (obj)newenv;
      f->index = i;
      val = vec_get(args, i * 2 - 3);
      env = newenv;
      goto eval;
    }
    else { // no more assignments
      f->type = FRAME_PROGN;
      f->args = cdr((cons*)f->args); // body
      f->index = 0;
      env = newenv;
      val = UNDEFINED;
      goto apply_k;
    }
  }

  case FRAME_VEC:
    vec_set((vector*)f->args, f->index++, val);
    if (f->index < vec_count((vector*)f->args)) {
      val = vec_get((vector*)f->args, f->index);
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
    if (type(f->args) == LVAL_NIL) {
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
    if (! type(arg) == LVAL_NUM) {                                       \
      return (obj)make_err("Arguments to " #sname " must be numbers");  \
    }                                                                   \
    number result = num_value(arg);                                     \
    for (int i = 3; i < vec_count(argv); i++) {                         \
      arg = vec_get(argv, i);                                           \
      if (type(arg) != LVAL_NUM) {                                       \
        return (obj)make_err("Arguments to " #sname " must be numbers"); \
      }                                                                 \
      result = result op num_value(arg);                                \
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
    if (type(arg) != LVAL_NUM) {                                         \
      return (obj)make_err("Arguments to " #sname " must be numbers");  \
    }                                                                   \
    number a = num_value(arg);                                          \
    for (int i = 3; i < vec_count(argv); i++) {                         \
      arg = vec_get(argv, i);                                           \
      if (type(arg) != LVAL_NUM) {                                       \
        return (obj)make_err("Arguments to " #sname " must be numbers"); \
      }                                                                 \
      number b = num_value(arg);                                        \
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

#define PRIM(name, func) (struct prim_s){name, &func};

vector* init_toplevel() {
  int c = 10;

  struct prim_s* prims = malloc(c * sizeof(struct prim_s));
  prims[0] = PRIM("+", prim_plus);
  prims[1] = PRIM("-", prim_minus);
  prims[2] = PRIM("*", prim_mult);
  prims[3] = PRIM("/", prim_div);
  prims[4] = PRIM("<", prim_lt);
  prims[5] = PRIM("<=", prim_lte);
  prims[6] = PRIM(">", prim_gt);
  prims[7] = PRIM(">=", prim_gte);
  prims[8] = PRIM("=", prim_numeq);
  prims[9] = PRIM("list", prim_list);

  // assume we won't do a collection here
  vector* names = make_vec(c);
  vector* env = make_vec(c + 2);
  vec_set(env, 1, (obj)names);

  for (int i=0; i < c; i++) {
    vec_set(names, i, (obj)make_sym(prims[i].name));
    vec_set(env, i + 2, (obj)make_prim(&prims[i]));
  }
  
  return env;
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

  vector* toplevel = (vector*)UNDEFINED;
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
