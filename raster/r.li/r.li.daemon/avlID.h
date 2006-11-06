
typedef struct avlID_node
{
     long id;
    long counter; 
    struct avlID_node *father; /* puntatore al padre */
    struct avlID_node *right_child; /* puntatore al figlio destro */
    struct avlID_node *left_child; /* puntatore al figlio sinistro */
}avlID_node;

typedef avlID_node*  avlID_tree;

/*tabella*/
typedef struct avlID_tableRow
{
     long k;
    long tot;
}avlID_tableRow;

typedef avlID_tableRow* avlID_table;

/* prototipi delle funzioni */
avlID_tree avlID_make( const  long k, const  long n);
avlID_node * avlID_find(const avlID_tree root, const   long k );
int avlID_add(avlID_tree *root, const  long k, const  long n);
long avlID_to_array(avlID_node * root,  long i, avlID_table *a);
long howManyID (const avlID_tree root, const   long k);
 long avlID_sub(avlID_tree *root, const  long k);

