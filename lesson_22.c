#include <stdio.h>
#include <stdlib.h>

/*------------------------ minimo intero libero ------------------------*/
int trova(int a[], int n, int x, int *p){
    for (int i = 0; i < n; i++)
        if (a[i] == x){*p = i; return 1;}
    return 0;
}

int minFree(int a[], int n){
    int p, i = -1;
    while (trova(a, n, ++i, &p)); // tutto viene fatto nella guardia del while
    return i;
}

int minFreeNonAnnidata(int a[], int n){
    int i = 0;
    int j = 0;
    while (i<n && j<n)
        if (a[i] == j) {i = 0; j++;}
            else i++;
    return j;
}

// funzione lineare allocando memoria
int minFreeLin(int v[], int n){
    int p[n+1]; // il min intero libero è in [0,n]
    azzera(p, n+1);

    for (int i = 0; i<n; i++)
        if (v[i] <= n) p[v[i]] = 1;

    for (int i = 0; i<n+1; i++)
        if (!p[i]) return i;
}

// soluzione divide et impera
int minFreeDeI(int v[], int inf, int sup){
    // REQ: x in [inf, sup]
    if (sup - inf == 1) //base
        if (v[inf] > inf) return inf;
            else return inf+1;
    
    p = puntoMedio(inf, sup);
    m = partiziona(v, inf, sup, p);
    if (m < p) return minFree(v, inf, p-1);
    return minFree(v, p, sup);
}

/*------------------------ allocazione di memoria ------------------------*/
/* 
    collocazione di memoria nell'heap con:
        void* malloc(int n): blocco di memoria di n bytes
        void* calloc(int k, int n): blocco di memoria di kxn
    tornano un puntatore alla base
    
    allocare memoria per un intero:
        int* n = (int *) malloc (sizeof(int))
                ^^^^^^^^ : coercion (assegna void* a int *)
    
    allocare memoria per un vetttore di n elementi:
        T* v = (T *) malloc (n*sizeof(T))
        T* v = (T *) calloc (n, sizeof(T))
    calloc azzera la memoria allocata
*/

/*
    Crivello di eratostene:
    Il risultato di tipo int* sarà l’indirizzo base di un vettore di 
    numeri primi lungo *k.
*/
int *generaPrimi(int n, int* k){
    int p[n+1];
    inizializza(p, n+1, 1); // inizializza p a tutti 1
    crivello(p, n+1);

    *k = contaUni(p, n+1);
    int *v = (int *) calloc(*k, sizeof(int));
    caricaPrimi(v, p, n+1);
    return v;
}

void crivello(int p[], int n){
    /*
        REQ: forall i in [0,n). p[i] = 1
        ENS: forall i in[0,n). p[i] <-> i è primo
    */
    for (int i = 2; i*i < n; i++)
    /*
        INV: forall k in [0,n)
        p[k] = 1 <-> k non divisibile per primi < i
    */
        if (p[i])
            for(int j = i*i; j<n; j+=2*i) p[i] = 0;
}

void caricaPrimi(int v[], int p[], int n){
    int j = 0;
    for (int i = 0; i<n; i++)
        if (p[i]) v[j++] = i;
}


/*------------------------ matrici ------------------------*/
/* 
    T a[m][n]: a[righe][colonne]
    tipo della matrice è T[][] = T**
    allocare m vettori di lunghezza n
    a[i][j] = a + (i * n+j) * sizeof(T)
*/

void* stampaMatrice(int a[][N], int m, int n){
    for (int i = 0; i < m; i++){
        for(int j = 0; j < n; j++)
            printf("%4d", a[i][j]);
        printf("\n");
    }
}

/*
    per allocare una matrice dinamica allocare un vettore di puntatori a vettori
    ognuno punta a un vettore riga della matrice
    per una matrice r x c:
        1. definire var a di tipo T**
        2. allocare un vettore di r elementi di tipo T*
        3. per ogni a[i] allocare un vettore riga
*/

// triangolo di tartaglia
int** triangoloTartaglia(int n){
    int **t = calloc(n, sizeof(int *));
    for (int i = 0; i < n; i++)
    {
        t[i] = calloc(i+1, sizeof(int));
        t[i][0] = 1;
        t[i][i] = 1;
        for (int j = 1; j < i; j++)
            t[i][j] = t[i-1][j-1] + t[i-1][j];
    }
    return t;
    
}

// costruzione di un quadrato magico
int **qmD(int n){
    int** q = allocM(n, n);
    int i, j;
    // prima pos è al centro della prima riga
    int x = 0;
    int y = n/2;
    int k = 1;
    
    do {
        q[x][y] = k++;
        i = x-1;
        j = y + 1;
        if (i < 0) i = n-1;
        if (j == n) j = 0;
        if (q[i][j] == 0) {x = i; y = j;}
            else x++;
    } while (k <= n * n);
    return q;
}

int **allocM(int r, int c){
    int** m = calloc(r, sizeof(int *));
    for (int i=0; i < r; i++)
        m[i] = calloc(c, sizeof(int));
    return m;
}

/*------------------------ main ------------------------*/
int main(int argc, char const *argv[]){
    int arr1[11] = {3, 8, 4, 5, 11, 15, 9, 2, 6, 0, 1};
    int arr2[6] = {4, 3, 6, 1, 0, 8};
    int res1 = minFree(arr1, 11);
    int res2 = minFreeNonAnnidata(arr2, 6);
    printf("Res1: %d, Res2: %d", res1, res2);
    return 0;
}