typedef struct
{
    int key;
    int data;
    int left;
    int right;
} BTREE_NODE;

typedef struct
{
    BTREE_NODE *node ;  /* tree of values */
    int tlen ;          /* allocated tree size */
    int N;              /* number of actual nodes in tree */
    int incr;		/* number of nodes to add at a time */
    int cur;
    int (*cmp)();	/* routine to compare keys */
} BTREE ;
