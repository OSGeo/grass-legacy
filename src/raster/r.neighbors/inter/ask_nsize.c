#include "gis.h"
#include "local_proto.h"

static char *explanation[] = {

"Please set the neighborhood size.",
"",
"  1 for the center cell alone",
"  3 for a 3 x 3 neighborhood around the center cell",
"  5 for a 5 x 5 neighborhood around the center cell",
"  7 for a 7 x 7 neighborhood around the center cell",
"  etc.",
"",
"enter <exit> if you want to exit",

0};

#define MAX 25

int ask_nsize (void)
{
    char **s;
    char buf[300];
    int nsize;

/*
 * ask user for neighborhood size.
 */

    for (s = explanation; *s; s++)
	fprintf (stdout,"%s\n", *s);

    while (1)
    {
	fprintf (stdout,"\nneighborhood size> ");
	if (!G_gets(buf)) continue;
	if (strcmp(buf,"exit") == 0)
	    exit(0);
	if(!scan_int(buf, &nsize))
	    continue;
	if (nsize <= 0)
	    fprintf (stdout,"must be greater than zero\n");
	else if (nsize%2 == 0)
	    fprintf (stdout,"must be an odd number\n");
	else if (nsize > MAX)
	    fprintf (stdout,"must be less than or equal to %d\n", MAX);
	else
	    break;
    }

    return nsize;
}
