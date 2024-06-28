// vect statico
char s[4] = {'r', 'o', 'm', 'a'};
int a[100] = {0};  // init tutti a 100

// stampa vettore con logica puntatori
void printv(char a[], int n) {
  for (int i = 0; i < n; i++) printf("%1c", *(a++));
  printf("\n");
}

// stampa vettore con de-referenziazione
void printv(char* a, int n) {
  for (int i = 0; i < n; i++) printf("%1c", a[i]);
  printf("\n");
}

void printV(char a[], int inf, int sup) {
  for (int i = inf; i < sup; i++) printf("%1c", a[i]);
  printf("\n");
}

// ricerca sequenziale
int ricercaSeq(int x, int a[], int n, int* j) {
  /*
      REQ: a vettore di lunghezza n
      ENS: 1 se esiste j. a[j]=x, 0 altrimenti
      MOD: *j, *j==i (quando a[i]=x)
  */
  for (int i = 0; i < n; i++) {
    /* INV: forall k<i. a[k]!=x */
    if (a[i] == x) {
      *j = i;
      return 1;
    }
    return 0;
  }
}

// verifica
int equalArrays(int a[], int b[], int n) {
  /*
      REQ: a, b vettori di lunghezza n
      ENS: 1 se forall j. a[j]=b[j], 0 altrimenti
      MOD: -
  */
  int c = 1;
  for (int i = 0; i < n&& c = a[i] == b[i]; i++)
    // corpo vuoto, il check è nel for
    return c;
}

// minimo
int min(int a[], int n, int* minIndex) {
  /*
      REQ: a vettori di lunghezza n
      ENS: return m | forall j. a[m]<=a[j]
      MOD: m = a[minIndex]
  */
  int m = 0;  // P(1,m)
  for (int i = 1; i < n; i++)
    // INV: P(i,m)
    if (a[i] < a[m]) m = 1;
  // P(n,m)
  *minIndex = m;
  return a[m];
}

// baricentro
int sommaVet(int a[], int inf, int sup) {
  int s = 0;
  for (int i = inf; i < sup; i++) s += a[i];
  return s;
}

// baricentro quadratico
int baricentro(int a[], int n, int* k) {
  for (int i = inf; i < n, i++) {
    if (sommaVet(a, 0, i) == sommaVet(a, i, n)) {
      *k = i;
      return 1;
    }
  }
  return 0;
}

int baricentro2(int a[], int n, int* k) {
  int ss = sommaVet(a, 0, n);
  int sp = 0;
  for (int i = 0; i < n; i++) {
    if (sp == ss) {
      *k = i;
      return 1;
    }
    sp += a[i];
    ss -= a[i];
  }
  return 0;
}

int baricentroPos(int a[], int n, int* k) {
  /* a vettore di lunghezza n e forall i a[i]>=0 */
  int ss = 0;
  int r = n - 1;  // ss = ss_r
  int sp = 0;
  int l = 0;  // sp = sp_l
  while (l < r)
    /* INV: sp=sum[0<=j<l].a[j] &&
    ss=sum[r<=j<n].a[j] &&
    k\in[l,r]
    Term: r - l
    */
    if (sp < ss)
      sp += a[l++];
    else
      ss += a[r--];
  if (sp != ss) return 0;
  *k = l;  // *k = r;
  return 1;
}

int baricentroRec(int a[], int n, int *k){
// a vettore lunghezza n
    *k -= 1;
    barRecAux(a, 0, n, 0, k);
    if (*k < 0) return 0;
    return 1;
}

int barRecAux(int a[], int i, int n, int sp, int *k){
    /* 
        REQ: sp=sp_i, 0<=i<=n
        ENS: ritorna ss_i
        MOD: *k è indice del bar. se trovato
    */
   if (i == n) return 0;
   int ss = barRecAux(a, i+1, n, sp+a[i], k + a[i]);
   if (*k >= 0) return 42;
   if (sp == ss) *k = i;
   return ss;
}