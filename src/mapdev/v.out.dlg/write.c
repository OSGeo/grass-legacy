/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include "digit.h"
#include "export_dlg.h"

#define DEF_MAJOR 999

static int ints_printed = 0;
static int atts_printed = 0;
static int coors_printed = 0;

start_ints ()
{
    ints_printed = 0;
}

write_ints (fp, num, ptr)
    FILE *fp;
    register int num;
    plus_t *ptr;
{
    register int i;
    for (i = 0 ; i < num ; i++)
    {
	fprintf (fp, "%6d", (int) ptr[i]);
	ints_printed++;
	if ((ints_printed % 12) == 0)
	    fprintf (fp, "\n"), ints_printed = 0;
    }
}

end_ints (fp)
    FILE *fp;
{
    if (ints_printed % 12)
	fprintf (fp, "\n");
}

start_att ()
{
    atts_printed = 0;
}

write_dlg_att (fp, major, minor)
    FILE *fp;
    int major, minor;
{
    fprintf (fp, "%6d%6d", major, minor);
    atts_printed++;
    if ((atts_printed % 6) == 0)
	fprintf (fp, "\n"), atts_printed = 0;
}
end_att (fp)
    FILE *fp;
{
    if (atts_printed % 6)
	fprintf (fp, "\n");
}



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
	fprintf (fp, "%12.2lf%12.2lf", Xptr[i], Yptr[i]);
	coors_printed++;
	if ((coors_printed % 3) == 0)
	    fprintf (fp, "\n"), coors_printed = 0;
    }
}

end_coords (fp)
    FILE *fp;
{
    if (coors_printed % 3)
	fprintf (fp, "\n");
}

