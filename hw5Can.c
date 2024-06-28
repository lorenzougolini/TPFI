#include <stdio.h>
#include <stdlib.h>

//esercizio 1
void checkEdian(){
    int x = 1; 
    //casto il puntatore l'indirizzo int (4byte) di x in un puntatore a char (1byte)
    char *cp = (char *) &x; 
    if(*cp == 1) 
        printf("Little endian\n"); 
    else  
        printf("Big endian\n"); 

}


//esercizio 2 

//compara per il valore
int compare_by_value(const void *a, const void *b) {
    int val1 = (*(int (*)[2])a)[0];
    int val2 = (*(int (*)[2])b)[0];
    return val1 - val2;
}
//compara per l'indice
int compare_by_index(const void *a, const void *b) {
    int id1 = (*(int (*)[2])a)[1];
    int id2 = (*(int (*)[2])b)[1];
    return id1 - id2;
}

//funzione per rimuovere duplicati
int remove_duplicates(int *arr, int n) {
    if (n == 0) return 0;

    int (*items)[2] = malloc(n * sizeof(int[2]));
    for (int i = 0; i < n; i++) {
        items[i][0] = arr[i]; // valore
        items[i][1] = i;      // indice
    }

    //sort per il valore
    qsort(items, n, sizeof(int[2]), compare_by_value);

    //rimuovi duplicati
    int j = 0;
    for (int i = 1; i < n; i++) {
        if (items[j][0] != items[i][0]) {
            j++;
            items[j][0] = items[i][0];
            items[j][1] = items[i][1];
        }
    }
    int new_size = j + 1;

    //sort per indice
    qsort(items, new_size, sizeof(int[2]), compare_by_index);

    // Copio indietro tutti i valori unici
    for (int i = 0; i < new_size; i++) {
        arr[i] = items[i][0];
    }

    free(items);
    return new_size;
}




//esercizio 3

//metodo per calcolare il coefficiente binomiale con programmazione dinamica
int binomialCoefficient(int n, int k) {
    
    int *C = (int *)malloc((k + 1) * sizeof(int));
    if (C == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }

    for (int i = 0; i <= k; i++) {
        C[i] = 0;
    }

    // Caso base, C(0, 0) = 1
    C[0] = 1;

    // Calcolo il coefficiente binomiale in modo iterativo
    for (int i = 1; i <= n; i++) {
        // j = i se i < k, altrimenti j = k
        for (int j = (i < k ? i : k); j > 0; j--) {
            C[j] = C[j] + C[j - 1];
        }
    }

    // assegnazione del risultato
    int result = C[k];
    free(C);

    return result;
}


//Esercizio 3

//Albero binario di ricorsione
typedef struct Node {
    int n, k;
    int value;
    struct Node *left;
    struct Node *right;
} Node;

//Costruttore per il nodo
Node* createNode(int n, int k, int value) {
    Node *node = (Node *)malloc(sizeof(Node));
    node->n = n;
    node->k = k;
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}

//Costruzione dell'albero binario di ricorsione
Node* buildRecursionTree(int n, int k) {
    if (k == 0 || k == n) {
        return createNode(n, k, 1);
    }
    Node *left = buildRecursionTree(n - 1, k - 1);
    Node *right = buildRecursionTree(n - 1, k);
    Node *node = createNode(n, k, left->value + right->value);
    node->left = left;
    node->right = right;
    return node;
}



//Grafo aciclico 
typedef struct GraphNode {
    int n, k;
    int value;
    struct GraphNode **children;
    int child_count;
} GraphNode;

//Costruttore per il nodo del grafo
GraphNode* createGraphNode(int n, int k, int value) {
    GraphNode *node = (GraphNode *)malloc(sizeof(GraphNode));
    node->n = n;
    node->k = k;
    node->value = value;
    node->children = NULL;
    node->child_count = 0;
    return node;
}

//Aggiungi un nodo al grafo e controlla se esiste già
GraphNode* addGraphNode(GraphNode **graph, int *graph_size, int n, int k, int value) {
    for (int i = 0; i < *graph_size; i++) {
        if (graph[i]->n == n && graph[i]->k == k) {
            return graph[i];
        }
    }
    GraphNode *node = createGraphNode(n, k, value);
    graph[(*graph_size)++] = node;
    return node;
}

//Soluzione più efficiente
GraphNode* buildDAG(int n, int k, GraphNode **graph, int *graph_size) {
    if (k == 0 || k == n) {
        return addGraphNode(graph, graph_size, n, k, 1);
    }
    GraphNode *left = buildDAG(n - 1, k - 1, graph, graph_size);
    GraphNode *right = buildDAG(n - 1, k, graph, graph_size);
    //controlla se il nodo esiste già
    GraphNode *node = addGraphNode(graph, graph_size, n, k, left->value + right->value);
    node->children = (GraphNode **)realloc(node->children, sizeof(GraphNode *) * (node->child_count + 2));
    node->children[node->child_count++] = left;
    node->children[node->child_count++] = right;
    return node;
}

//printa il root node per l'albero binario di ricorsione
void printNode(Node *node) {
    printf("Node(n=%d, k=%d, value=%d)\n", node->n, node->k, node->value);
}

//printa il root node per il grafo aciclico
void printGraphNode(GraphNode *node) {
    printf("GraphNode(n=%d, k=%d, value=%d)\n", node->n, node->k, node->value);
}



//ESERCIZIO 4 CRIVELLA EULERO

//Struttura per memorizzare i valori
typedef struct {
    int succ; 
    int prec; 
} Pair;

//inizializza il vettore
Pair* initializeVector(int n) {
    Pair* pairs = (Pair*)malloc((n + 1) * sizeof(Pair));
    for (int i = 0; i <= n; ++i) {
        pairs[i].succ = 1;
        pairs[i].prec = 1;
    }
    return pairs;
}

//Algoritmo per trovare i numeri primi
Pair* eulerSieve(int n) {
    //inizializzo il vettore
    Pair* pairs = initializeVector(n);
    //setto 0 e 1 come non primi
    pairs[0].succ = pairs[0].prec = 0; 
    pairs[1].succ = pairs[1].prec = 0; 

    //inizio il loop da 2, se il numero è primo setto tutti i suoi multipli come non primi
    for (int i = 2; i <= n; ++i) {
        if (pairs[i].succ != 0) { 
            for (int j = 2 * i; j <= n; j += i) {
                pairs[j].succ = 0; 
            }
        }
    }

    //calcolo il gap dei prev
    int prev = -1;
    for (int i = 0; i <= n; ++i) {
        if (pairs[i].succ != 0) {
            if (prev != -1) {
                pairs[prev].succ = i - prev;
                pairs[i].prec = i - prev;
            }
            prev = i;
        }
    }
    return pairs;
}

//printa i numeri primi
void printPrimes(Pair* pairs, int n) {
    for (int i = 2; i <= n; ++i) {
        if (pairs[i].succ != 0) {
            printf("%d ", i);
        }
    }
    printf("\n");
}


int main() {
    checkEdian();

    char str[100];
    sprintf(str, "%d", binomialCoefficient(40,30));
    printf("Binomial Coefficient C(40, 30) = %s\n", str);

    int arr[] = {4, 2, 1, 4, 3, 2, 4, 1, 5, 2};
    int n = sizeof(arr) / sizeof(arr[0]);

    int new_size = remove_duplicates(arr, n);

    for (int i = 0; i < new_size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");

    printPrimes(eulerSieve(76), 76);

    return 0;
}