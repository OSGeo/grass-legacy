#include <stdio.h>
#include "ibtree.h"

char *malloc();
char *realloc();

ibtree_update (B, key, data)
    IBTREE *B;
    int key;
    int data;
{
    register int p,q;
    int N;
    int (*cmp)();
    int dir;
    char *store();

/* first node is special case */
    N = B->N;
    if (N == 0)
    {
	N = B->N = 1;
	B->node[N].key = key;
	B->node[N].data = data;
	B->node[N].left = 0;
	B->node[N].right = 0;
	/*
	if (B->node[N].key && B->node[N].data)
	*/
	    return 1;
	/*
	else
	    return 0;
	*/
    }

    cmp = B->cmp;

    q = 1;
    while (q > 0)
    {
	p = q;
	dir = (*cmp)(B->node[q].key, key) ;
	if (dir == 0)
	{
	    /*
	    free (B->node[q].data);
	    */
	    B->node[q].data = data;
	    return 1;
	}
	if (dir > 0)
	    q = B->node[q].left;             /* go left */
	else
	    q = B->node[q].right;            /* go right */
    }

/* new node */
    B->N++;
    N = B->N;

/* grow the tree? */
    if (N >= B->tlen)
    {
	B->node = (IBTREE_NODE *) realloc (B->node, sizeof(IBTREE_NODE) * (B->tlen += B->incr));
	if (B->node == NULL)
	    return 0;
    }

/* add node to tree */
    B->node[N].key = key;
    B->node[N].data = data;
    B->node[N].left = 0;

    if (dir > 0)
    {
	B->node[N].right = -p;            /* create thread */
	B->node[p].left  = N;             /* insert left */
    }
    else
    {
	B->node[N].right = B->node[p].right; /* copy right link/thread */
	B->node[p].right = N;                /* add right */
    }
    return 1;
}

static
char *
store (s, n)
    char *s;
    register int n;
{
    register char *b,*c;

    if (n <= 0)
	return (b = NULL);
    c = b = malloc (n);
    if (b == NULL)
	return b;
    while (n-- > 0)
	*b++ = *s++;
    return c;
}
