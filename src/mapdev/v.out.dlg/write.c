/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include "Vect.h"
#include "export_dlg.h"

#define DEF_MAJOR 999

static int ints_printed = 0;
static int atts_printed = 0;
static int coors_printed = 0;

int start_ints (void)
{
    ints_printed = 0;

    return 0;
}

int write_ints ( FILE *fp,
    register int num, plus_t *ptr)
{
    register int i;
    for (i = 0 ; i < num ; i++)
    {
	fprintf (fp, "%6d", (int) ptr[i]);
	ints_printed++;
	if ((ints_printed % 12) == 0)
	    fprintf (fp, "\n"), ints_printed = 0;
    }

    return 0;
}

int end_ints ( FILE *fp)
{
    if (ints_printed % 12)
	fprintf (fp, "\n");

    return 0;
}

int start_att (void)
{
    atts_printed = 0;

    return 0;
}

int write_dlg_att ( FILE *fp,
    int major,int minor)
{
    fprintf (fp, "%6d%6d", major, minor);
    atts_printed++;
    if ((atts_printed % 6) == 0)
	fprintf (fp, "\n"), atts_printed = 0;

    return 0;
}

int end_att ( FILE *fp)
{
    if (atts_printed % 6)
	fprintf (fp, "\n");

    return 0;
}



int start_coords (void)
{
    coors_printed = 0;

    return 0;
}

int write_coords (
    FILE *fp, register int num,
    double *Xptr,double *Yptr)
{
    register int i;
    for (i = 0 ; i < num ; i++)
    {
	fprintf (fp, "%12.2f%12.2f", Xptr[i], Yptr[i]);
	coors_printed++;
	if ((coors_printed % 3) == 0)
	    fprintf (fp, "\n"), coors_printed = 0;
    }

    return 0;
}

int end_coords ( FILE *fp)
{
    if (coors_printed % 3)
	fprintf (fp, "\n");

    return 0;
}

