// Code from https://github.com/blynn/dlx
// Copyright Ben Lynn
//
// See http://en.wikipedia.org/wiki/Dancing_Links.
#include <limits.h>
#include <stdlib.h>
#include <setjmp.h>
#include "dlx.h"

#define F(i,n) for(int i = 0; i < n; i++)

#define C(i,n,dir) for(cell_ptr i = (n)->dir; i != n; i = i->dir)

struct cell_s;
typedef struct cell_s *cell_ptr;
struct cell_s {
  cell_ptr U, D, L, R;
  int n;
  union {
    cell_ptr c;
    int s;
  };
};

// Some link dance moves.
static cell_ptr LR_self(cell_ptr c) { return c->L = c->R = c; }
static cell_ptr UD_self(cell_ptr c) { return c->U = c->D = c; }

// Undeletable deletes.
static cell_ptr LR_delete(cell_ptr c) {
  return c->L->R = c->R, c->R->L = c->L, c;
}
static cell_ptr UD_delete(cell_ptr c) {
  return c->U->D = c->D, c->D->U = c->U, c;
}

// Undelete.
static cell_ptr UD_restore(cell_ptr c) { return c->U->D = c->D->U = c; }
static cell_ptr LR_restore(cell_ptr c) { return c->L->R = c->R->L = c; }

// Insert cell j to the left of cell k.
static cell_ptr LR_insert(cell_ptr j, cell_ptr k) {
  return j->L = k->L, j->R = k, k->L = k->L->R = j;
}

// Insert cell j above cell k.
static cell_ptr UD_insert (cell_ptr j, cell_ptr k) {
  return j->U = k->U, j->D = k, k->U = k->U->D = j;
}

static cell_ptr col_new(void) {
  cell_ptr c = malloc(sizeof *c);
  if (!c) abort();
  UD_self(c)->s = 0;
  return c;
}

struct dlx_s {
  int ctabn, rtabn, ctab_alloc, rtab_alloc;
  cell_ptr *ctab, *rtab;
  cell_ptr root;
};

dlx_t dlx_new(void) {
  dlx_t p = malloc(sizeof *p);
  if (!p) abort();
  p->ctabn = p->rtabn = 0;
  p->ctab_alloc = p->rtab_alloc = 8;
  p->ctab = calloc(p->ctab_alloc, sizeof(cell_ptr));
  p->rtab = calloc(p->rtab_alloc, sizeof(cell_ptr));
  p->root = LR_self(col_new());
  return p;
}

void dlx_clear(dlx_t p) {
  // Elements in the LR list for each row are never covered, thus all cells
  // can be accessed from the 'rtab' LR lists.
  F(i, p->rtabn) {
    cell_ptr r = p->rtab[i];
    if (r) {
      cell_ptr next;
      for(cell_ptr j = r->R; j != r; j = next) {
        next = j->R;
        free(j);
      }
      free(r);
    }
  }
  // Columns may be covered, but they are always accessible from 'ctab'.
  F(i, p->ctabn) free(p->ctab[i]);
  free(p->rtab);
  free(p->ctab);
  free(p->root);
  free(p);
}

int dlx_rows(dlx_t dlx) { return dlx->rtabn; }
int dlx_cols(dlx_t dlx) { return dlx->ctabn; }

static void dlx_add_col(dlx_t p) {
  cell_ptr c = col_new();
  LR_insert(c, p->root);
  c->n = p->ctabn++;
  if (p->ctabn == p->ctab_alloc) {
    p->ctab = realloc(p->ctab, sizeof(cell_ptr) * (p->ctab_alloc *= 2));
    if (!p->ctab) abort();
  }
  p->ctab[c->n] = c;
}

static void dlx_add_row(dlx_t p) {
  if (p->rtabn == p->rtab_alloc) {
    p->rtab = realloc(p->rtab, sizeof(cell_ptr) * (p->rtab_alloc *= 2));
    if (!p->rtab) abort();
  }
  p->rtab[p->rtabn++] = 0;
}

static void alloc_col(dlx_t p, int n) { while(p->ctabn <= n) dlx_add_col(p); }
static void alloc_row(dlx_t p, int n) { while(p->rtabn <= n) dlx_add_row(p); }

void dlx_mark_optional(dlx_t p, int col) {
  alloc_col(p, col);
  cell_ptr c = p->ctab[col];
  // Prevent undeletion by self-linking.
  LR_self(LR_delete(c));
}

static cell_ptr new1(cell_ptr c, int row) {
  cell_ptr n = malloc(sizeof *n);
  if (!n) abort();
  n->n = row;
  n->c = c;
  c->s++;
  UD_insert(n, c);
  return n;
}

void dlx_set(dlx_t p, int row, int col) {
  // We don't bother sorting. DLX works fine with jumbled rows and columns.
  // We just have to watch out for duplicates. (Actually, I think the DLX code
  // works even with duplicates, though it would be inefficient.)
  //
  // For a given column, the UD list is ordered in the order that dlx_set()
  // is called, not by row number. Similarly for a given row and its LR list.
  alloc_row(p, row);
  alloc_col(p, col);
  cell_ptr c = p->ctab[col];
  cell_ptr *rp = p->rtab + row;
  if (!*rp) {
    *rp = LR_self(new1(c, row));
    return;
  }
  // Ignore duplicates.
  if ((*rp)->c->n == col) return;
  C(r, *rp, R) if (r->c->n == col) return;
  // Otherwise insert at end of LR list.
  LR_insert(new1(c, row), *rp);
}

static void cover_col(cell_ptr c) {
  LR_delete(c);
  C(i, c, D) C(j, i, R) UD_delete(j)->c->s--;
}

static void uncover_col(cell_ptr c) {
  C(i, c, U) C(j, i, L) UD_restore(j)->c->s++;
  LR_restore(c);
}

int dlx_pick_row(dlx_t p, int i) {
  if (i < 0 || i >= p->rtabn) return -1;
  cell_ptr r = p->rtab[i];
  if (!r) return 0;  // Empty row.
  cover_col(r->c);
  C(j, r, R) cover_col(j->c);
  return 0;
}

void dlx_solve(
  dlx_t p,
  void *dat,
  void (*try_cb)(void*, int, int, int),
  void (*undo_cb)(void*),
  void (*found_cb)(void*),
  void (*stuck_cb)(void*, int))
{
  cell_ptr c = p->root->R;
  if (c == p->root) {
    if (found_cb) found_cb(dat);
    return;
  }
  int s = INT_MAX;  // S-heuristic: choose first most-constrained column.
  C(i, p->root, R) if (i->s < s) s = (c = i)->s;
  if (!s) {
    if (stuck_cb) stuck_cb(dat, c->n);
    return;
  }
  cover_col(c);
  C(r, c, D) {
    if (try_cb) try_cb(dat, c->n, s, r->n);
    C(j, r, R) cover_col(j->c);
    dlx_solve(p, dat, try_cb, undo_cb, found_cb, stuck_cb);
    if (undo_cb) undo_cb(dat);
    C(j, r, L) uncover_col(j->c);
  }
  uncover_col(c);
}

struct forall_st {
  void (*cb)(int *, int);
  int *soln;
  int used;
  int single;
  jmp_buf env;
};

static void forall_try(void *dat, int col, int s, int row) {
  struct forall_st *st = dat;
  st->soln[st->used++] = row;
}

static void forall_undo(void *dat) {
  struct forall_st *st = dat;
  st->used--;
}

static void forall_found(void *dat) {
  struct forall_st *st = dat;
  st->cb(st->soln, st->used);
  if (st->single) {
    longjmp(st->env, 1);
  }
}

void dlx_forall(dlx_t p, int single, void (*cb)(int *, int)) {
  struct forall_st st =
    { .soln = calloc(dlx_rows(p), sizeof(int))
    , .cb = cb
    , .used = 0
    , .single = single
    };
  if (!st.soln) abort();
  if (!single || !setjmp(st.env)) {
    dlx_solve(p, &st, forall_try, forall_undo, forall_found, NULL);
  }
  free(st.soln);
}
