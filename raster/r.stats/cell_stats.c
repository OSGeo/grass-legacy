#include "global.h"

int cell_stats (int fd[], int verbose, int with_counts,
    int with_areas, int with_labels, char *fmt)
{
    CELL **cell;
    int i;
    int row;
    double unit_area;
    int planimetric = 0;
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
	{
	    if (G_get_c_raster_row (fd[i], cell[i], row) < 0)
		exit(1);
           reset_null_vals(cell[i], ncols);
        /* we can't compute hash on null values, so we change all
	   nulls to max+1, set NULL_CELL to max+1, and later compare with 
	   NULL_CELL  to chack for nulls */
        }

	update_cell_stats (cell, ncols, unit_area);
    }
    if (verbose)
	G_percent (nrows, nrows, 2);

    sort_cell_stats();
    print_cell_stats (fmt, with_counts, with_areas, with_labels, fs);

    return 0;
}
