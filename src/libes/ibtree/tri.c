#include "ibtree.h"

static int int_cmp (int *,int *);

static IBTREE B;

int Btree_init ()
{
    ibtree_create (&B, int_cmp, 10);
}

int Btree_add (int key)
{
    int *k, *d, *cur;
    int strcmp();

    {
	if (ibtree_find (&B,(char *) &key,&d))
	{
	    cur = d+1;
	}
	else
	{
	    cur = 1;
	}
	ibtree_update (&B, &key, sizeof (int), &cur, sizeof (int));
    }
}

int Btree_report ()
{
    fprintf (stdout,"final tree\n");
    ibtree_rewind (&B);
    while (ibtree_next (&B, &k, &d))
	fprintf (stdout,"%02x%02x%02x:%6d\n", k & 0xff, (k >> 8) & 0xff, 
	    (k >> 16) & 0xff, d);
}


static int int_cmp (int *a,int *b)
{
    if (a < b)
	return (-1);
    if (a > b)
	return (1);
    return (0);
}
