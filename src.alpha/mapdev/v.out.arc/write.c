#include "digit.h"
#include "gtoa.h"

static int coors_printed = 0;

start_coords ()
{
    coors_printed = 0;
}

write_coords (fp, num, Xptr, Yptr)
FILE *fp;
register int num;
double *Xptr, *Yptr;
{
    register int i;
    for (i = 0 ; i < num ; i++)
    {
	fprintf (fp, "%12.2lf%c%12.2lf\n", Xptr[i], separator, Yptr[i]);
	coors_printed++;
    }
}

end_coords (fp)
    FILE *fp;
{
}

