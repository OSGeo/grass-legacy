#include "global.h"

cell_stats (fd, verbose, non_zero, with_counts, with_areas, with_labels, fmt)
    int fd[];
    char *fmt;
{
    CELL **cell;
    int i;
    int row;
    double unit_area;
    int planimetric;
    int compute_areas;
    double G_area_of_cell_at_row();

/* allocate i/o buffers for each cell file */
    cell = (CELL **) G_calloc (nfiles, sizeof (CELL *));
    for (i = 0; i < nfiles; i++)
	cell[i] = G_allocate_cell_buf();

/* if we want area totals, set this up.
 * distinguish projections which are planimetric (all cells same size)
 * from those which are not (e.g., lat-long)
 */
    unit_area = 0.0;
    if (with_areas)
    {
	switch (G_begin_cell_area_calculations())
	{
	case 0: /* areas don't make sense, but ignore this for now */
	case 1:
	    planimetric = 1;
	    unit_area = G_area_of_cell_at_row (0);
	    break;
	default:
	    planimetric = 0;
	    break;
	}
    }
    compute_areas = with_areas && !planimetric;

/* here we go */
    initialize_cell_stats(nfiles);
    if (verbose)
	fprintf (stderr, "%s: ", G_program_name());

    for (row = 0; row < nrows; row++)
    {
	if (compute_areas)
	    unit_area = G_area_of_cell_at_row (row);

	if (verbose)
	    G_percent (row, nrows, 2);

	for (i = 0; i < nfiles; i++)
	    if (get_row (fd[i], cell[i], row) < 0)
		exit(1);

/* read the mask file */
	if (maskfd >= 0)
	    G_get_map_row_nomask (maskfd, mask, row);

	update_cell_stats (cell, mask, ncols, unit_area);
    }
    if (verbose)
	G_percent (nrows, nrows, 2);

    sort_cell_stats();
    print_cell_stats (fmt, non_zero, with_counts, with_areas, with_labels, fs);
}
