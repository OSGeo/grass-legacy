#include "Vect.h"
#include "gtoa.h"

static int coors_printed = 0;

int start_coords (void)
{
    coors_printed = 0;
    return 0;
}

int write_coords (FILE *fp, register int num, double *Xptr, double *Yptr)
{
    register int i;
    for (i = 0 ; i < num ; i++)
    {
	fprintf (fp, "%12.2f%c%12.2f\n", Xptr[i], separator, Yptr[i]);
	coors_printed++;
    }
    return 0;
}

int end_coords (FILE *fp)
{
      return 0;
}

