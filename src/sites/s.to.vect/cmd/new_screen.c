#include <stdio.h>
#include "gis.h"

/* %W% %G% */
int new_screen (void)
{
    G_clear_screen ();
    fprintf (stdout,"SITES to VECT\n\n");

    return 0;
}
