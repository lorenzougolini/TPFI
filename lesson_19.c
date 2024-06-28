#include <stdio.h>
int x = 0;

int f() {return x;}

void g(int x) {x++;}

int main (int argc, char* argv[]){
    // commenti
    /* commenti */
    printf("hello world\n");
    int y = f();
    g(x);
    printf("%d\n", x);
    return 0;
}

/*------------------------ tiny C --------------------
Cominciamo con un frammento minimale del C, contenente if, 
while, sequenza, assegnazioni e variabili intere.
*/

int somma(int m, int n){
    /*  REQ: m, n >= 0      assunzioni sui parametri
        ENS: ritorna m+n    condizioni sui risultati
        MOD: -              eventuali side-effects
    */
   int i = 0;
   while (i++ != n) m++; 
   // prima calcola il valore dell'espressione logica, poi incrementa
   // i++ è un comando. tutti i comandi restituiscono un valore, il valore assegnato
   /*
   if (x=0) printf("pippo"); else printf("pluto"); valuterà sempre false
   */
   return m;
}

int sommaRec(int m, int n){
    return sommaRecAux(m, n, 0);
}
int sommaRecAux(int m, int n, int i){
    if (i == n) return m;
    return sommaRecAux(m+1, n, i+1);
}
// oppure
int sommaRecVPC(int m, int n){
    if (!n) return m;
    return sommaRecVPC(m+1, n-1); // non funzionerebbe usando il post incremento/decremento m++/n--
}


// predecessore
#include<assert.h>
int pred(int n){
    assert(n > 0);
    int i = 0;
    int j = 1;
    while (j != n){
        // INV: j = i + 1
        i++;
        j++;
    }
    return i;
}

int pred(int n){
    /*
    REQ: n > 0
    ENS: return n - 1
    */
    assert(n > 0);
    int i = 0;
    while (i+1 != n) i++;
    // INV: i < n
    return i;
}
// questo codice torna -1 se eseguito con 0

int div(int m, int n){
    // REQ: n > 0    
    int q = 0;
    int r = m;
    while (r>=n){
        // INV: m = q * n + r
        r -=n;
        q++;
    }
    return q;
}

int compareTo(int m, int n){
    /*
    m>n -> 1 
    m=n -> 0
    m<n -> -1
    REQ: m,n >= 0
    */
    if (m == n) return 0;
    while (m && n){
        m = pred(m);
        n = pred(n);
    }
    if (n) return -1;
    return 1;
}

// seconda versione contando in avanti
int compareTo(int m, int n){
    // REQ: m,n >= 0
    int i = 0;
    if (m==n) return 0;
    while (42)    {
        if (i==m) return -1;
        if (i==n) return 1;
        i++;
    }
}

/* divisione intera:
m = qn + r
q=0 e r = m lo soddisfa
incrementando q di 1: q’n’ + r’ = (q+1)n + (r - n) = qn + n + r - n = qn + r
r ≥ n è la guardia naturale
*/
int div(int m, int n){
    // REQ: n > 0
    int q = 0;
    int r = m;
    while (r>=n){
        // INV: m = q*n+r
        r -= n;
        q++;
    }
    return q;
}

/* moltiplicazione egiziana:
m * 2n = (m + m) * n    (n>1)
m * (2n + 1) = m * 2n + m
m * 0 = 0
*/
int multEgypt(int m, int n){
    // base
    if (!n) return 0;
    // dispari
    if (n % 2) return m + multEgypt(m, n-1);
    // pari
    return multEgypt(m+m, n/2);
}

/* per vatiante iterativa usare accumulatore p
tiene invariato p + mn = m0n0
init p = 0
n dispari: p’+ m’n’ = p + m + m(n-1) = p + m + mn - m = p + mn = m0n0
n pari: p’+ m’n’ = p + 2m(n/2) = p + mn = m0n0
n != 0 è guardia
*/
int multEgyptIter(int m, int n){
    int p = 0;
    while (n){
        /*
        INV: r + m * n = m0 * n0
        T = n
        */
        if (n%2) {
            p += m;
            n--;
        } else{
            m *= 2;
            n /= 2;
        }
    }
    return p;
}
