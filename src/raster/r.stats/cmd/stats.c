#include "global.h"

/* hash definitions (these should be prime numbers) **************/
#define HASHSIZE 7307
#define HASHMOD  89

static CELL *values;
static struct Node *node_pool;
static int node_pool_count;
static CELL *value_pool;
static int value_pool_count;
#define NODE_INCR 32
#define VALUE_INCR 32

static struct Node **sorted_list;

struct Node
{
    CELL *values;
    struct Node *left;
    struct Node *right;
    struct Node *list;
    long count;
    double area;
} ;

static struct Node **hashtable;
static struct Node *node_list = NULL;
static int node_count = 0;

initialize_cell_stats(n)
{
    int i;

	/* record nilfes first */
    nfiles = n;

	/* allocate a pool of value arrays */
    value_pool_count = 0;
    allocate_values();

	/* set Node pool to empty */
    node_pool_count = 0;


	/* empty the has table */
    hashtable = (struct Node **) G_malloc (HASHSIZE * sizeof(struct Node *));
    for (i = 0; i < HASHSIZE; i++)
	hashtable[i] = NULL;
}

allocate_values()
{
    value_pool_count = VALUE_INCR;
    value_pool = (CELL *) G_calloc (nfiles * value_pool_count, sizeof(CELL));
    values = value_pool;
}

struct Node *NewNode(area)
    double area;
{
    struct Node *node;

    if (node_pool_count <= 0)
	node_pool = (struct Node *) G_calloc (node_pool_count = NODE_INCR, sizeof(struct Node));
    node = &node_pool[--node_pool_count];
    node->count = 1;
    node->area = area;
    node->values = values;

    if (--value_pool_count <= 0)
	allocate_values();
    else
	values += nfiles;

    node->left = node->right = NULL;
    node->list = node_list;
    node_list = node;
    node_count++;

    return node;
}

update_cell_stats(cell,mask,ncols,area)
    CELL **cell, *mask;
    double area;
{
    register int i;
    register int hash;
    register struct Node *p,*q;
    register int dir;

    while (ncols-- > 0)
    {
		/* masked values are ignored */

	if (!mask[ncols])
	    continue;

	    /* copy this cell to an array, compute hash */

	hash = values[0] = cell[0][ncols];
	for (i = 1; i < nfiles; i++)
	    hash = hash * HASHMOD + (values[i] = cell[i][ncols]);
	if (hash < 0)
	    hash = -hash;
	hash %= HASHSIZE;

	    /* look it up and update/insert */

	if ((q = hashtable[hash]) == NULL)
	{
	    hashtable[hash] = NewNode(area);
	}
	else
	{
	    while(1)
	    {
		for (i = 0; i < nfiles; i++)
		{
		    if (values[i] < q->values[i])
		    {
			dir = -1;
			p = q->left;
			break;
		    }
		    if (values[i] > q->values[i])
		    {
			dir = 1;
			p = q->right;
			break;
		    }
		}
		if (i == nfiles) /* match */
		{
		    q->count++;
		    q->area += area;
		    break;
		}
		else if (p == NULL)
		{
		    if (dir < 0)
			q->left = NewNode(area);
		    else
			q->right = NewNode(area);
		    break;
		}
		else
		    q = p;
	    }
	}
    }
}

node_compare(p,q)
    register struct Node **p, **q;
{
    register int i, x;
    register CELL *a, *b;
    a = (*p)->values;
    b = (*q)->values;
    for (i = nfiles; --i >= 0; )
	if (x = (*a++ - *b++))
	    return x;
    return 0;
}

sort_cell_stats()
{
    struct Node **q, *p;

    if (node_count <= 0) return;

    free(hashtable); /* make a bit more room */
    sorted_list = (struct Node **) G_calloc (node_count, sizeof(struct Node *));
    for (q = sorted_list, p = node_list; p; p = p->list)
	*q++ = p;
    
    qsort (sorted_list, node_count, sizeof(struct Node *), node_compare);
}

print_node_count()
{
    printf ("%d nodes\n", node_count);
}

print_cell_stats (fmt, non_zero, with_counts, with_areas, with_labels, fs)
    char *fmt;
    char *fs;
{
    int i,n;
    struct Node *node;

    if (node_count <= 0)
    {
	printf ("0");
	for (i = 1; i < nfiles; i++)
	    printf ("%s0",fs);
	if (with_areas)
	    printf ("%s0.0",fs);
	if (with_counts)
	    printf ("%s0",fs);
	if (with_labels)
	    printf ("%s%s", fs, G_get_cat ((CELL) 0, &labels[i]));
	printf ("\n");
    }
    else
    {
	for (n = 0; n < node_count; n++)
	{
	    node = sorted_list[n];

	    if (non_zero)
	    {
		for (i = 0; i < nfiles; i++)
		    if (node->values[i])
			break;
		if (i == nfiles)
		    continue;
	    }

	    for (i = 0; i < nfiles; i++)
	    {
		printf ("%s%ld", i?fs:"", (long) node->values[i]);
		if (with_labels)
		    printf ("%s%s", fs,
			G_get_cat ((CELL) node->values[i], &labels[i]));
	    }
	    if (with_areas)
	    {
		printf ("%s", fs);
		printf (fmt, node->area);
	    }
	    if (with_counts)
		printf ("%s%ld", fs, (long) node->count);
	    printf ("\n");
	}
    }
}
