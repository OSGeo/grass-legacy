#include "gis.h"
#include "site.h"

mask_sites (sites_old, sites_new, fd, mask, ncats)

	SITE_LIST *sites_old, *sites_new;
	int fd;
	char mask[];
	CELL ncats;
{
	struct Cell_head window;
	CELL *cellbuf, cat;
	int n;

	int row, col;	/* location of site */

	double north, east;
	char *desc;

	float northing_to_row ();
	float easting_to_col ();

	G_get_window (&window);

	printf("%d sites ... ", count_sites (sites_old,0));
	fflush (stdout);
	rewind_site_list (sites_old);


	cellbuf   = G_allocate_cell_buf () ;

	counter_reset ("", 1);

	for (n = 1; next_site (sites_old, &north, &east, &desc); n++)
	{
		counter (n);

		row = northing_to_row (north, &window);
		col = easting_to_col (east, &window);

		if (row < 0 || row >= window.rows ||
		    col < 0 || col >= window.cols)
			continue;

		if(G_get_map_row (fd, cellbuf, row) < 0)
		{
			printf ("** error reading mask file **\n");
			return 0;
		}

		cat = cellbuf[col];
		if (cat < 0 || cat > ncats) continue;
		if (mask[cat])
			add_site (sites_new, north, east, desc);
	}
	printf("\n");
	G_close_cell (fd);
	free (cellbuf);

	return 1;
}
