#include "report.h"
#include "local_proto.h"

int do_layer (FILE *out, REPORT *ref,int layer_num, int *cats_list)
{
    int i,n;
    int next;
    int cur;
    int count;

    n = ref->matrix.size;
    if (n <= 0) return 0;

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

    return 0;
}
