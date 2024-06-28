#include <stdio.h>
#include <stdlib.h>

/*
    1. Type Safety
    L’esistenza del tipo void* è il più evidente segno del fatto che in C sia un
    linguaggio non type-safe, nonostante la buona volontà dei compilatori moderni
    che hanno proibito diverse possibilità presenti nel C standard delle origini.
    Scrivere un programma Cche dà risultati diversi se compilato su un’archi-
    tettura big-endian (il primo byte è il più significativo, tipico di Motorola, Sun,
    IBM. . . ) oppure su una little-endian (il primo byte è il meno significativo,
    tipico di Intel e Digital. . . ).
*/

int archCheck() {
    int num = 1;
    char *byte = (char*) &num;
    if (byte[0])
        printf("Little Endian\n");
    else
        printf("Big Endian\n");
    return 0;
}

/*
    2. Array Mutabili
    Scrivere una funzione C di complessità Θ(n log n) che elimina i duplicati da un
    vettore (analogo dell’esercizio 1 dell’Homework 1), compattando gli elementi
    a sinistra, e restituendo come risultato il numero degli elementi rimasti.
    Potete ovviamente usare strutture dati per mimare la vostra soluzione Ha-
    skell, ma quale potrebbe essere il modo migliore per rendere reversibile un
    ordinamento di un array in C?
    Riflettere bene sul prototipo corretto della funzione, in accordo con il ga-
    lateo del C e riflettere anche su quali potrebbero essere (e come definirle)
    eventuali strutture dati ausiliarie.
    L’algoritmo elementare per risolvere questo problema è Θ(n^2), dove n è la
    lunghezza della lista.
*/

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int removeDups (int *arr, int n){
    qsort(arr, n, sizeof(int), compare);
    
    int j = 0;

    for (int i = 0; i < n; i++)
        if (i == 0 || arr[i] != arr[i-1])
            arr[j++] = arr[i];
    
    return j;
}


/*  
    3. Quello che in Haskell non si può fare I: Dati “non-funzionali”
    Considerate la seguente funzione ricorsiva che calcola i coefficienti binomiali.
        int cbin(int n, int k){
            if (n==k || k==0) return 1;
            return cbin(n-1,k-1)+cbin(n-1,k);
        }
    Dare prima la definizione di un tipo cBinTree adatto a memorizzare in un
    albero binario i valori dei parametri n e k e il valore calcolato dalla funzione in tale
    chiamata (ogni nodo di un cbinTree contiene quindi tre valori.).
    Scrivere poi una funzione C che genera l’albero delle chiamate ricorsive
    della funzione int cbin(int n, int k), memorizzando in ogni nodo i valori
    dei parametri n e k e il valore calcolato dalla funzione in tale chiamata (vedi Fig. 1).
    Siccome ci sono numerosi sotto-alberi ripetuti nell’albero delle chiamate, è
    interessante costruire il grafo aciclico delle chiamate ricorsive, ma questa volta
    allocando un unico nodo per eventuali chiamate ripetute (con gli stessi valori
    per i parametri n e k), evitando quindi la replicazione di sotto-alberi (vedi
    Fig. 2). Osservate che in questo caso ci sono cammini diversi che finiscono
    nello stesso nodo e osservate anche che non occorre cambiare la definizione del
    tipo cBinTree, né funzioni di stampa o di visita.
*/

typedef struct cBinTree {
    int n;
    int k;
    int res;
    struct cBinTree* left;
    struct cBinTree* right;
} cBinTree;

cBinTree* createNode(int n, int k){
    cBinTree* node = (cBinTree*) malloc(sizeof(cBinTree));

    node->n = n;
    node->k = k;
    node->res = 0;

    node->left = NULL;
    node->right = NULL;
    return node;
}

cBinTree* cbin(int n, int k){
    if (n == k || !k){
        cBinTree* newNode = createNode(n, k);
        newNode->res = 1;
        return newNode;
    }

    cBinTree* newNode = createNode(n, k);

    cBinTree* sx = cbin(n-1, k-1);
    cBinTree* rx = cbin(n-1,k);

    newNode->left = sx;
    newNode->right = rx;    
    newNode->res = sx->res + rx->res;
    return newNode;
}

/* 
    versione aciclica
*/

typedef struct listNode {
    int n;
    int k;
    cBinTree* treeNode;
    struct listNode* next;
} listNode;


cBinTree* findNode(listNode* L, int n, int k){
    while (L){
        if (L->n == n && L->k == k)
            return L->treeNode;
        L = L->next;
    }
    return NULL;
}

void insertNode(listNode** L, int n, int k, cBinTree* treeNode){
    listNode* newListNode = (listNode*) malloc(sizeof(listNode));
    newListNode->n = n;
    newListNode->k = k;
    newListNode->treeNode = treeNode;
    newListNode->next = *L;
    *L = newListNode;
}

cBinTree* cbinAcilico(int n, int k, listNode** L){
    
    cBinTree* existingNode = findNode(*L, n, k); 
    if (existingNode)
        return existingNode;
    
    if (n == k || !k){
        cBinTree* newNode = createNode(n, k);
        newNode->res = 1;
        insertNode(L, n, k, newNode);
        return newNode;
    }

    cBinTree* newNode = createNode(n, k);

    cBinTree* sx = cbinAcilico(n-1, k-1, L);
    cBinTree* rx = cbinAcilico(n-1, k, L);

    newNode->left = sx;
    newNode->right = rx;    
    newNode->res = sx->res + rx->res;

    insertNode(L, n, k, newNode);
    return newNode;
}


/*
    4. Quello che in Haskell non si può fare II: Crivello di Eulero
    Non è ovvio “saltare” in modo efficiente i numeri già cancellati per trarne vantaggio nelle
    successive cancellazioni. La soluzione che vi propongo di implementare con-
    siste nell’usare un vettore di coppie di naturali, succ e prec, come una lista
    doppiamente concatenata in cui nella posizione i, se i non è stato cancellato,
    succ è il numero di posizioni che occorre saltare per andare al prossimo nu-
    mero non cancellato, mentre prec è il numero di posizioni che occorre saltare
    (all’indietro) per andare al precedente numero non cancellato.
    Potete ad esempio definire un tipo Pair che è una coppia di interi succ e
    prec e definiamo un vettore di Pair. Questo vettore va inizializzato con tutti 1
    (che significa appunto che tutti i numeri sono ancora potenziali primi).
*/

typedef struct Pair {
    int succ;
    int pred;
} Pair;



void init(Pair* p, int n){
    for (int i = 2; i < n; i++){
        p[i].succ = 1;
        p[i].pred = 1;
    }
}

void delete(Pair* p, int j, int n) {

}

Pair* eulerSieve(int n){
    Pair* p = (Pair*) malloc(n * sizeof(Pair));
    init(p, n);

    for (int i = 2; i < n; i++) {
        if (p[i].succ) {
            for (int j = i * 2; j < n; j += i) {
                if (j - p[j].pred > 0) {
                    p[j - p[j].pred].succ += p[j].succ;
                }
                if (j + p[j].succ < n) {
                    p[j + p[j].succ].pred += p[j].pred;
                }
                // Mark non-prime
                p[j].succ = p[j].pred = 0;
            }
        }
    }
    return p;
}

void printPrimes(Pair* p, int n) {
    for (int i = 2; i <= n; ++i) {
        if (p[i].succ) {
            printf("%d ",i);
        }
    }
    printf("\n");
}

/*---------------------------------------------------------------------------------*/

int main(){

    archCheck();

    int arr[] = {4, 2, 2, 3, 1, 4, 3, 5};
    int n = sizeof(arr) / sizeof(arr[0]);

    printf("Array originale: ");
    for (int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("\n");

    int newSize = removeDups(arr, n);

    printf("Array senza duplicati ha dimensione %d: ", newSize);
    for (int i = 0; i < newSize; i++)
        printf("%d ", arr[i]);
    printf("\n");

    int n1 = 40;
    int k1 = 35;
    cBinTree* r1 = cbin(n1, k1);
    printf("Risultato normale C(%d, %d): %d", n1, k1, r1->res);
    printf("\n");

    int n2 = 40;
    int k2 = 30;
    listNode* list = NULL;
    cBinTree* r2 = cbinAcilico(n2, k2, &list);
    printf("Risultato aciclico C(%d, %d): %d", n2, k2, r2->res);
    printf("\n");

    int size = 507;
    Pair* p = eulerSieve(size + 1);

    printPrimes(p, size);

    return 1;
}