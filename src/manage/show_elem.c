/* %W% %G% */

#include "list.h"

show_elements ()
{
    int n;
    int len;

    fprintf (stderr, "known database elements are:\n");
    len = 0;
    for (n = 0 ; n < nlist; n++)
	if (strlen (list[n].alias) > len)
	    len = strlen (list[n].alias);
    for (n = 0 ; n < nlist; n++)
	fprintf (stderr, "  %-*s (%s)\n", len, list[n].alias, list[n].text);
}
