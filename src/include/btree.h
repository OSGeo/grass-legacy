typedef struct
{
    char *key;
    char *data;
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

/* create.c */
int btree_create(BTREE *, int (*)(), int);
/* find.c */
int btree_find(BTREE *, char *, char **);
/* free.c */
int btree_free(BTREE *);
/* next.c */
int btree_next(BTREE *, char **, char **);
/* rewind.c */
int btree_rewind(BTREE *);
/* update.c */
int btree_update(BTREE *, char *, int, char *, int);
