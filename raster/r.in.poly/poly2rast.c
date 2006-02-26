#include <stdlib.h>
#include "gis.h"
#include "format.h"
#include "local_proto.h"

int poly_to_rast (char *input_file, char *raster_map, char *title, int nrows)
{
    double *x, *y;
    int count;
    long cat;
    int type;
    struct Categories labels;
    FILE *ifd;	/* for input file */
    int rfd;	/* for raster map */
    int format;
    int stat;
    int pass, npasses;

    ifd = fopen (input_file, "r");
    if (ifd == NULL)
    {
	perror (input_file);
	exit(1);
    }

    rfd = G_open_cell_new (raster_map);
    if (rfd < 0)
    {
	fprintf (stderr, "ERROR: Can't create raster map <%s>", raster_map);
	exit(1);
    }

    if (title == NULL) title = "";
    G_strip(title);
    G_init_cats ((CELL) 0, title, &labels);
    format = getformat(ifd);
    npasses = begin_rasterization(nrows, format);
    pass = 0;
    do
    {
	pass++;
	if (npasses > 1)
	{
	    fprintf (stdout,"Pass #%d (of %d) ... ", pass, npasses);
	    fflush(stdout);
	}

	fseek (ifd, 0L, 0);
	while (get_item(ifd, &type, &cat, &x, &y, &count, &labels))
	{
	    set_cat(cat);
	    switch(type)
	    {
	    case 'A':
		G_plot_polygon (x, y, count);
		break;
	    case 'L':
		while (--count > 0)
		{
		    G_plot_line2 (x[0],y[0],x[1],y[1]);
		    x++;
		    y++;
		}
		break;
	    }
	}

	fprintf (stdout,"writing raster map ... "); fflush (stdout);
	stat = output_raster(rfd);
    } while (stat == 0);
    /* stat: 0 means repeat
     *       1 means done
     *      -1 means error
     */

    if (stat < 0)
    {
	G_unopen_cell(rfd);
	return 1;
    }

    fprintf (stdout,"Creating support files for raster map <%s>\n", raster_map);
    G_close_cell(rfd);
    G_write_cats (raster_map, &labels);

    fprintf (stdout,"Done\n");
    return 0;
}
