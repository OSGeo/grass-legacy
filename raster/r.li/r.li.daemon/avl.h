#ifndef AVL_H
#define AVL_H

#include "GenericCell.h"

typedef struct avl_node{
    generic_cell key; /* il campo key e' costituito da un valore che puo' essere CELL, DCELL o FCELL */
    long counter; /* il dato contenuto nel nodo e' un intero che serve da contatore */
    struct avl_node *father; /* puntatore al padre */
    struct avl_node *right_child; /* puntatore al figlio destro */
    struct avl_node *left_child; /* puntatore al figlio sinistro */
}avl_node;

typedef avl_node*  avl_tree;

/*tabella*/
typedef struct AVL_tableRow{
    generic_cell k;
    long tot;
}AVL_tableRow;

typedef AVL_tableRow* AVL_table;

/* prototipi delle funzioni */
avl_tree avl_make( const generic_cell k, const long n);
avl_node * avl_find(const avl_tree root, const generic_cell k);
int avl_add(avl_tree *root, const generic_cell k, const long n);
long avl_to_array(avl_node * root, long i, AVL_table *a);
long howManyCell (const avl_tree root, const generic_cell k);


#endif
