#include "gis.h"
#include "site.h"

int site_data (FILE *meta_report, SITE_LIST *site_list,
    char *layer, char *mapset, int layer_num,
    struct Cell_head *window, int quadsize)
{
    int fd;
    CELL *cellbuf;
    CELL value;
    int n;

    int row, col;		/* for quad around site */
    int site_row, site_col;	/* location of site itself */
    int r, c;   		/* offset from center of site */

    double north, east;
    char *desc;

    float northing_to_row ();
    float easting_to_col ();


    fprintf (stdout,"%-15s ... ", layer); fflush (stdout);


    if ((fd = G_open_cell_old (layer, mapset)) < 0)
    {
	fprintf (stdout,"** error - can't open raster file **\n");
	return -1;
    }

    cellbuf   = G_allocate_cell_buf () ;

    counter_reset ("", 0);

    rewind_site_list (site_list);
    for (n = 1; next_site (site_list, &north, &east, &desc); n++)
    {
/* if I include this line, G_get_map_row returns incorrect values
	counter (n);
*/

	site_row = (int) northing_to_row (north, window);
	site_col = (int) easting_to_col (east, window);

	fprintf(meta_report,"data|%d|%d", layer_num, n);
	for (r = -quadsize; r <= quadsize; r++)
	{
	    row = r + site_row;
	    if (row >= 0 && row < window->rows)
		G_get_map_row (fd, cellbuf, row);
	    for (c = -quadsize; c <= quadsize; c++)
	    {
		col = c + site_col;
		if (row >= 0 && row < window->rows &&
		    col >= 0  && col < window->cols)
			value = cellbuf[col];
		else
			value = 0;

		fprintf(meta_report,"|%ld", (long)value);
	    }
	}
	fprintf(meta_report,"\n");
    }
    fprintf (stdout,"\n");
    G_close_cell (fd);
    G_free (cellbuf);

    return 0;
}
