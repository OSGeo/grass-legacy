#include "gis.h"
#include "icon.h"

plot_points (icon, reversed)
    ICON *icon;
{
    double east, north;
    int x,y;
    int line;
    char buf1[100], buf2[100];
    char *ebuf, *nbuf;
    char buf[1024];
    int projection;

    projection = G_projection();

    if (reversed)
    {
	nbuf = buf1;
	ebuf = buf2;
    }
    else
    {
	ebuf = buf1;
	nbuf = buf2;
    }
    for (line = 1; input(buf, reversed); line++)
    {
	*buf1 = *buf2 = 0;
	sscanf (buf, "%s %s", buf1, buf2);
	if (*buf1 == 0 || *buf1 == '#') continue;
	if (!G_scan_northing (nbuf, &north, projection) 
	||  !G_scan_easting (ebuf, &east, projection) )
	{
	    if (!isatty(0))
		fprintf (stderr, "line %d invalid: %s\n", line, buf);
	    else
		fprintf (stderr, "** illegal input, ignored **\n");
	    continue;
	}
	G_plot_where_xy (east, north, &x, &y);
	draw_icon (icon, x, y);
    }
}

input(buf, reversed)
    char *buf;
{
    static int first = 1;
    if (isatty(0))
    {
	if (first)
	{
	    printf ("enter point coordinates, or end to finish\n");
	    first = 1;
	}
	if (reversed)
	    printf ("north east> ");
	else
	    printf ("east north> ");
    }
    if (!gets(buf)) return 0;
    G_strip(buf);
    return (strcmp(buf, "end") != 0);
}
