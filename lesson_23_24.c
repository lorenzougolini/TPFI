struct AP {
  char nome[10];
  char cognome[10];
  char dataNascita;
  char luogoNascita;
};

// definire nuovo nome di tipo con typedef
typedef struct D {
  int anno;
  int mese;
  int giorno;
} data;

int dm[13] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
void inserisciData(data* d) {
  do {
    printf("\ninserire giorno: ");
    scanf("%d", &(d->giorno));
    printf("\ninserire mese: ");
    scanf("%d", &(d->mese));
    printf("\ninserire anno: ");
    scanf("%d", &(d->anno));
  } while (!legale(*d));
}

int legale(data d) {
  int maxg;

  if (d.anno < 0 || d.mese < 1 || d.mese > 12 || d.giorno < 1) return 0;

  if (d.mese == "feb" && bisestil(d.anno))
    maxg = 29;
  else
    maxg = dm[d.mese];

  if (d.giorno > maxg) return 0;

  return 1;
}

/*-------------------- LISTE --------------------*/

#include <stdio.h>
#include <stdlib.h>

typedef struct L {
  int val;
  struct L* next;  // usa il puntatore per determinare la memoria necessaria
} listanode;

typedef listanode* lista;  // tipo lista è un tipo pointer

lista cons(int x, lista L) {
  // mem allocation per nuovo nodo
  lista M = (lista)malloc(sizeof(listanode));
  M->val = x;   // M = [x];
  M->next = L;  // M = x:L;
  return M;
}

int isEmpty(lista L) {
  if (L == NULL) return 1;
  return 0;
}

int head(lista L) { return L->val; }

int tail(lista L) { return L->next; }

// se lista non vuota estrae testa e coda
int isNotEmpty(lista L, int* x, int* xs) {
  if (L) {
    *x = L->val;
    *xs = L->next;
    return 1;
  } else
    return 0;
}

int length(lista L) {
  lista xs;
  int x;
  if (isNotEmpty(L, &x, &xs)) return 1 + length(xs);
  return 0;
}
int length2(lista L) {
  if (!L) return 0;
  return 1 + length(L->next);
}

int maxL(lista L) {
  if (isEmpty(tail(L))) return head(L);
  return max(head(L), maxL(tail(L)));
}
int maxL2(lista L) {
  if (!L->next) return L->val;
  return max(L->val, maxL(L->next));
}

int sum(lista L) {
  if (isEmpty(L)) return 0;
  return head(L) + sum(tail(L));
}
int sum2(lista L) {
  if (!L) return 0;
  return L->val + sum(L->next);
}

void twiceRec(lista L) {
  if (L) {
    L->val *= 2;
    twiceRec(L->next);
  }
}

// twice funzionale
lista twiceFun(lista L) {
  if (!L) return L;
  return cons(2 * L->val, twiceFun(L->next));
}

// questa funzione non fa niente
void tail2(lista L) { L = L->next; }
// questa funzione stacca la coda
void cutTail(lista L) { L->next = NULL; }

// twice iterativa
void twiceIt(lista L) {
  lista lPtr = L;
  while (lPtr) {
    lPtr->val *= 2;
    lPtr = lPtr->next;
  }
}

int tiwceFunIt(lista L) {
  // caso vuoto
  if (!L) return L;
  // creo primo nodo da tornare
  lista res = cons(2 * L->val, NULL);
  // non perdere la testa di res
  lista aux = res;
  L = L->next;
  while (L) {
    // creo nuovo nodo e attacco
    aux->next = cons(2 * L->val, NULL);
    // avanzo sulle liste
    L->next;
    aux = aux->next;
  }
  return res;
}

lista addTailFun(lista L, int y) {
  if (!L) return cons(y, NULL);
  return cons(L->val, addTailFun(L->next, y));
}

lista addTailRec(int x, lista L) {
  // una sola chiamata a cons
  if (!L) return cons(x, NULL);

  // un'assegnazione sempre inutile tranne quando arriva in fondo
  L->next = addTailRec(x, L->next);
  return L;
}

int addTailIt(int x, lista L) {
  lista lAux = L;
  // creo nuovo nodo da inserire
  lista tmp = cons(x, NULL);
  if (lAux) {
    // posizione sull'ultimo nodo
    while (lAux->next) lAux = lAux->next;
    // aggangio ultimo nodo
    lAux->next = tmp;
    return L;
  } else
    return tmp;
}

lista append(lista L, lista M) {
  if (!L) return M;
  return cons(L->val, append(L->next, M));
}

int main() {
  lista L = cons(1, cons(2, cons(3, NULL)));
  addTailRec(4, L);
  return 0;
}