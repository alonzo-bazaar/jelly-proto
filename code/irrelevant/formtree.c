#include<stdio.h>
#include<stdlib.h>

/* any clue I have on how the form builder might work is too statey
 * so I'll throw some ideas together in C and then see how may I port it to haskell
 * and once I've ported it to haskell I can work on porting the ideas from haskell to java
 * and once I'm done with that I'll probably rewrite it all in rust
 * because three languages were not enough for a toy interpreter
 */

/* BEGIN Node */
struct Node {
    char c;
    struct Node* left;
    struct Node* right;
};
typedef struct Node Node;

Node* make_node(char c, Node* left, Node* right) {
    Node* n = (Node*)malloc(sizeof(Node));
    n->c = c;
    n->left = left;
    n->right = right;
    return n;
}
Node* shit_node(char c) {
    return make_node(c,NULL,NULL);
}
void free_node(Node* n) {
    if(n -> left) free_node(n->left);
    if(n -> right) free_node(n->right);
    free(n);
}
/* END Node */

/* BEGIN parse the motherfucker
 * currently no check for string validity, those might be needed */
void node_append(Node* n, Node* a) {
    Node* next = n->right;
    while(next) {
        n = next;
        next=next->right;
    }
    n->right = a;
}
void node_append_char(Node* n, char c) {
    node_append(n, shit_node(c));
}

/* this screams
 * "HEY! make a struct so you can keep track of this while processing
 * you're getting the same info twice
 * this is both inefficient and a middle finger to dry
 * please"
 */
int advance_until_balance(int i, char* c) {
    int depth = (c[i] == '('?1:0);
    while(depth) {
        i++;
        if(c[i] == '\0') return -1;
        else if(c[i] == '(') depth++;
        else if(c[i] == ')') depth--;
    }
    return i;
}

Node* last_from(Node* n) {
    Node* next = n->right;
    while(next) {
        n = next;
        next = next->right;
    }
    return n;
}
    
/* what haskell does to a mf */
Node* make_form(char* str, int i) {
    Node* acc = NULL;
    while(1) {
        switch (str[i]) {
        case'\0': return acc;
        case ')': return acc;
        case '(':
            if(acc)
                last_from(acc)->left=make_form(str,i+1);
            else
                acc = make_node(' ', NULL, make_form(str, i+1));
            i = 1+advance_until_balance(i, str); /* inefficient as shit */
            break;
        default:
            if(acc)
                node_append_char(acc, str[i]);
            else
                acc = shit_node(str[i]);
            i++;
        }
    }
}
/* END parse the motherfucker (boi I can see the segs fault) */

void print_node(Node* n) {
    if(n==NULL) {
        puts("nil");
        return;
    }
    putchar('(');

    if (n->c) printf(":char #\\%c\n", n->c);

    printf(":left "); print_node(n->left);
    printf(":right "); print_node(n->right);

    putchar(')');
}

int main(int argc, char** argv) {
    Node* n = make_form(argv[1],0);
    print_node(n);
    /* Node* n = make_form("(defun(dio)(cristo)dr((u))()ido)"); */
    return 0;
}
    
