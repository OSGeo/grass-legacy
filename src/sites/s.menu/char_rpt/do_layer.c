#include "report.h"

do_layer (out, ref, layer_num, cats_list)
    FILE *out;
    REPORT *ref;
    int *cats_list;
{
    int i,n;
    int next;
    int cur;
    int count;

    n = ref->matrix.size;
    if (n <= 0) return;

    cur = cats_list[0];
    count = 0;
    for (i = 0; i < n; i++)
    {
	next = cats_list[i];
	if (next != cur)
	{
	    do_cat (out, ref, layer_num, cur, count, n);
	    count = 0;
	    cur = next;
	}
	count++;
    }
    do_cat (out, ref, layer_num, cur, count, n);
}
