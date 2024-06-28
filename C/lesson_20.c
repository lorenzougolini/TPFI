void provaPuntatori(){
    int m = 3;
    int n = 2;
    int p;
    int* x = &p;
    int* y = NULL;
    int* z = &m;
    (*z)++; 
    // il ++ fa saltare alla prossima cella di memoria disponibile in base al tipo
    y = &m;
    z = &p;
    p = 7;
    (*z)++;
    x++;
}

void scambia(int a, int b){
    int h = a;
    a = b;
    b = h;
    // così non si cambiano i valori del chiamante
}

// scambia(&a, &b)
void scambia(int *a, int *b){
    int h = *a;
    *a = *b;
    *b = h;
}

/* questa funzione non funziona
    - genera overflow e underflow
    - se i due puntatori sono uguali (stessa cella): *a=*a-*b mette entrambi a 0
*/
void scambia(int *a, int *b){
    *a = *a - *b;
    *b = *b + *a;
    *a = *b - *a;
}

int minimo(int a[], int inf, int sup){
    /* 
    REQ: inf < sup
    ENS: ritorna m, forall j.inf<=j<sup
    a[m]<=a[j]
     */
}
int selectionSort(int a[], int n){
    for (int i = 0; i < n; i++)
    {
        int m = minimo(a, i, n);
        scambia(&a[i], &a[m]);
    }
}

// soluzione in selectionSort
void selectionSort(int a[], int n){
    for (int i=0; i<n; i++){
    int m = minimo(a, i, n);
    if (i!=m) scambia(&a[i], &a[m]);
    }
}

// soluzione in scambia
void scambia(int *a, int *b){
    if (*a != *b){
        *a = *a - *b;
        *b = *b + *a;
        *a = *b - *a;
    }   
}
void selectionSort(int a[], int n){
    for (int i=0; i<n; i++)
        scambia(&a[i], &a[minimo(a, i, n)]);
}

int div(int m, int n, int* q, int* r){
    /* 
        REQ:n > 0 
        ENS:ritorna 1 se n divide m
        MOD:*q(quoziente),*r(resto) di m/n
    */
    int *q = 0;
    int *r = n;
    while (*r >= n){
        *r -= n;
        *q++;
    }
    return (*r == 0);
}

#define min(A, B) (A)>(B) ? (B) : (A)
int mcdMaestra(int m, int n){
    /* 
        REQ: m, n > 0 
        ENS:ritorna MCD(m,n)
    */
    int mcd = 1;
    int p = 2; // prossimo candidato

    int q1, q2, r1, r2;
    while (p < min(m,n)){
        /* INV: mcd*MCD(m,n)=MCD(m0,n0) */
        if (div(m,p,&q1,&r1)) m = q1;
        if (div(n,p,&q2,&r2)) n = q2;
        if (r1 && r2) p++;
        if (!r1 && !r2) mcd *= p;
    }
    return mcd;
}

int assParallelo(int* x, int* y, int u, int v){
    /* MOD: *x e *y prenderanno i valori di u e v */
    *x = u;
    *y = v;
}