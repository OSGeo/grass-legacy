#include "gis.h"
#include "report.h"

/* from the current report data record, determine the most frequently
   occuring category.  If the site center category occurs with equal
   frequency, insure that this center category is returned.

   note: category zero is excluded
*/

long mode (REPORT *report)
{
    struct Cell_stats statf;
    int i;
    long max;
    long count;
    CELL mode;
    CELL cat;
    long atol();

    G_init_cell_stats (&statf);

    for (i = 3; i < report->nfields; i++)
    {
	if(cat = atol(report->field[i]))
	    G_update_cell_stats (&cat, 1, &statf);
    }
    mode = 0;
    max = 0;
    G_rewind_cell_stats (&statf);
    while (G_next_cell_stat (&cat, &count, &statf))
	if (count > max)
	{
	    mode = cat;
	    max = count;
	}

    if (report->matrix.center >= 0)
    {
	cat = atol(report->field[report->matrix.center+3]);
	if (G_find_cell_stat (cat, &count, &statf) && (count >= max))
		mode = cat;
    }

    G_free_cell_stats (&statf);
    return mode;
}
