#include "report.h"

int do_cat (FILE *out,REPORT *ref,int layer_num,
    int cat,int cell_count,int layer_count)
{
    if (layer_count != 1)
    {
	fprintf (out, "    %4ld cell%s in ", (long)cell_count,
		cell_count==1 ? " " : "s");
    }
    else
	fprintf (out, "                  ");

    fprintf (out, "cat %-4ld ", (long)cat);

    if (report_find_cat (ref, layer_num, cat))
	    fprintf (out, "%s", ref->field[4]);
    fprintf (out, "\n");

    return 0;
}
