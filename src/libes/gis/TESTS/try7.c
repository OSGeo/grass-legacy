#include "gis.h"

main()
{
    char name[30], *mapset;
    struct Histogram h;
    struct Colors colr;
    int i;
    int red, grn, blu;
    CELL cat;
    int ncats;
    long count;
    long sum;

    G_gisinit ("");
    mapset = G_ask_cell_old ("", name);
    if (mapset == NULL) exit(0);

printf ("reading histogram\n");
    if(G_read_histogram (name, mapset, &h) <0)
	exit(0);
printf ("reading colors\n");
    if(G_read_colors (name, mapset, &colr) <0)
	exit(0);

    sum = 0;
    ncats = G_get_histogram_num (&h);
    for (i = 0; i < ncats; i++)
    {
	cat = G_get_histogram_cat (i, &h);
	count = G_get_histogram_count (i, &h);
	sum += count;
	G_get_color (cat, &red, &grn, &blu, &colr);
	printf ("%ld:%ld=%ld (%d,%d,%d)\n", (long) cat, count, sum, red, grn,blu);
    }
}
