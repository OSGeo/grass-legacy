#include "gis.h"
static char *parms[] =
{
    "n=value s=value e=value w=value",
    "res=value nsres=value ewres=value",
    "window=name save=name default",
    "layer=name zoom=name align=name",
    "print (or) gprint",
    0
};
usage ()
{
    int i;
    fprintf (stderr, "usage: %s [-] parms\n", G_program_name());
    fprintf (stderr, "where valid parms are:\n");
    for (i = 0; parms[i]; i++)
	fprintf (stderr, "  %s\n", parms[i]);
    exit(1);
}
