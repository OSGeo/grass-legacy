#include "report.h"

do_cat (out, ref, layer_num, cat, cell_count, layer_count)
    FILE *out;
    REPORT *ref;
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
}
