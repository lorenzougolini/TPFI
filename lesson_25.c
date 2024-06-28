#include <stdio.h>

typedef struct B {
    struct B* left;
    int info;
    struct B* right;
} binTreeNode;

typedef binTreeNode* binTree;

binTree makeLeaf(int r){
    binTree B;
    B = (binTree) malloc(sizeof(binTreeNode));
    B->info = r;
    B->left = NULL;
    B->right = NULL;
    return B;
}

binTree makeTree(int r, binTree L, binTree R){
    binTree B = makeLeaf(r);
    B->left = L;
    B->right = R;
    return B;
}

int isEmptyBT(binTree B, int* r, binTree* L, binTree* R){
    if (!B) return 1;
    *r = B->info;
    *L = B->left;
    *R = B->right;
    return 0;
}

int nodes(binTree B){
    int r;
    binTree L, R;
    if(isEmptyBT(B, &r, &L, &R))
        return 0;
    return 1 + nodes(L) + nodes(R);
}

/*------------------- TIPI GENERICI ---------------------*/
typedef struct BG {
    struct BG* left;
    void* info; // diventa pointer anche il campo info
    struct BG* right;
} binTreeGNode;

typedef binTreeGNode* binTreeGen;


/* definizione STACK */
typedef struct S {
    void* val;
    struct S* next;
} Snode;

typedef struct {
    Snode* top;
    int numElem;
} stackDescriptor;

typedef stackDescriptor* stack;

stack createEmptyStack(){
    stack S;
    S = malloc(sizeof(stackDescriptor));

    S->top = NULL;
    S->numElem = 0;
    return S;
}

int isEmpty(stack S){
    return S->numElem == 0;
}

void* top(stack S){
    return (S->top)->val;
}

void pop(stack S){
    Snode *tmp;
    S->numElem--;
    tmp = S->top;
    S->top = S->top->next;
    free(tmp);
}

void push(stack S, void* el){
    Snode* tmp;
    tmp = malloc(sizeof(Snode));
    
    tmp->next = S->top;
    tmp->val = el;
    
    S->top = tmp;
    S->numElem++;
}