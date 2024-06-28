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

lista addTailRec(int x, lista L) {
  // una sola chiamata a cons
  if (!L) return cons(x, NULL);

  // un'assegnazione sempre inutile tranne quando arriva in fondo
  L->next = addTailRec(x, L->next);
  return L;
}

void printList(lista L) {
  if (L) {
    printf("%d ", L->val);
    printList(L->next);
  }
} 

void divideFun(lista L, lista *D, lista *P){
    if (L) {
        divide2Fun(L->next, D, P);
        *D = cons(L->val, *D);
    } else { *D = NULL; *P = NULL;}
}
void divide2Fun(lista L, lista *D, lista *P){
    if (L) {
        divideFun(L->next, D, P);
        *P = cons(L->val, *P);
    } else { *D = NULL; *P = NULL;}
}

void divideSwap(lista L, lista *D, lista *P){
    if (L) {
        divideSwap(L->next, P, D);
        *D = cons(L->val, *D);
    } else { *D = NULL; *P = NULL;}
}

int palindromAux(lista L, lista* lAux){
    if (!L) return 1;
    if (!palindromAux(L->next, lAux) || L->val != (*lAux)->val)
        return 0;
    *lAux = (*lAux)->next;
    return 1;
}

int palindromaRec(lista L){
    return palindromAux(L, &L);
}

int main() {
    lista L = cons(1, cons(2, cons(3, NULL)));
    lista D = NULL;
    lista P = NULL;
    // divideFun(L, &D, &P);
    divideSwap(L, &D, &P);
    printList(L);
    printf("\n");
    printList(D);
    printf("\n");
    printList(P);
    printf("\n");

    lista M = cons(1, cons(2, cons(1, NULL)));
    palindromaRec(M) ? printf("Palindroma\n") : printf("Non palindroma\n");

    return 0;
}