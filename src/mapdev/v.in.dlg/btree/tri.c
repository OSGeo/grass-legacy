#include "btree.h"

int int_cmp ();

static BTREE B;

Btree_init ()
{
    btree_create (&B, int_cmp, 10);
}

Btree_add (key)
    int key;
{
    int *k, *d, *cur;
    int strcmp();

    {
	if (btree_find (&B,(char *) &key,&d))
	{
	    cur = d+1;
	}
	else
	{
	    cur = 1;
	}
	btree_update (&B, &key, sizeof (int), &cur, sizeof (int));
    }
}

Btree_report ()
{
    printf ("final tree\n");
    btree_rewind (&B);
    while (btree_next (&B, &k, &d))
	printf ("%02x%02x%02x:%6d\n", k & 0xff, (k >> 8) & 0xff, 
	    (k >> 16) & 0xff, d);
}


int_cmp (a, b)
    int *a, *b;
{
    if (a < b)
	return (-1);
    if (a > b)
	return (1);
    return (0);
}
