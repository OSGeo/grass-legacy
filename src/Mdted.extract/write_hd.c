/* %W% %G% */
#include "dma.h"

write_hd()
{
    fprintf (headfd, "north: ");
    printgeo(headfd, north,"SN");
    fprintf (headfd, "\n");

    fprintf (headfd, "south: ");
    printgeo(headfd, south,"SN");
    fprintf (headfd, "\n");

    fprintf (headfd, "east:  ");
    printgeo(headfd, east,"EW");
    fprintf (headfd, "\n");

    fprintf (headfd, "west:  ");
    printgeo(headfd, west,"EW");
    fprintf (headfd, "\n");

    fprintf (headfd, "latres: %d", latres/10);
    if (latres%10)
	fprintf (headfd, ".%d", latres%10);
    fprintf (headfd, "\n");

    fprintf (headfd, "lonres: %d", lonres/10);
    if (lonres%10)
	fprintf (headfd, ".%d", lonres%10);
    fprintf (headfd, "\n");

    fprintf (headfd, "rows: %d\n", nrows);
    fprintf (headfd, "cols: %d\n", ncols);
    fprintf (headfd, "bytes per col: 2\n");

    fclose (headfd);
}
