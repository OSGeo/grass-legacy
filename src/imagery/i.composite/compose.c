#include "globals.h"

compose ()
{
    int *make_lookup();
    CELL *in_buf, *out_buf;
    int red_fd, grn_fd, blu_fd;
    int out_fd;
    char *name, *mapset;
    int row, nrows, col, ncols;

    red_fd = grn_fd = blu_fd = -1;

    if (ref.red.n >= 0)
    {
	RED = make_lookup (ref.red.table, ref.red.min, ref.red.max, r_level, g_level*b_level);


	name = ref.file[ref.red.n].name;
	mapset = ref.file[ref.red.n].mapset;
	red_fd = G_open_cell_old (name, mapset);
	if (red_fd < 0)
	    exit(1);
    }

    if (ref.grn.n >= 0)
    {
	GRN = make_lookup (ref.grn.table, ref.grn.min, ref.grn.max, g_level, b_level);


	name = ref.file[ref.grn.n].name;
	mapset = ref.file[ref.grn.n].mapset;
	grn_fd = G_open_cell_old (name, mapset);
	if (grn_fd < 0)
	    exit(1);
    }

    if (ref.blu.n >= 0)
    {
	BLU = make_lookup (ref.blu.table, ref.blu.min, ref.blu.max, b_level, 1);


	name = ref.file[ref.blu.n].name;
	mapset = ref.file[ref.blu.n].mapset;
	blu_fd = G_open_cell_old (name, mapset);
	if (blu_fd < 0)
	    exit(1);
    }

    out_fd = G_open_cell_new (result);
    if (out_fd < 0)
	exit(1);

    in_buf = G_allocate_cell_buf ();
    out_buf = G_allocate_cell_buf ();


    nrows = G_window_rows();
    ncols = G_window_cols();

    fprintf (stderr, "%s: ", G_program_name());
    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 2);

	for (col = 0; col < ncols; col++)
	    out_buf[col] = 1;

	if (red_fd >= 0)
	{
	    G_get_map_row (red_fd, in_buf, row);
	    paint (out_buf, in_buf, ncols, RED, ref.red.min, ref.red.max);
	}

	if (grn_fd >= 0)
	{
	    G_get_map_row (grn_fd, in_buf, row);
	    paint (out_buf, in_buf, ncols, GRN, ref.grn.min, ref.grn.max);
	}

	if (blu_fd >= 0)
	{
	    G_get_map_row (blu_fd, in_buf, row);
	    paint (out_buf, in_buf, ncols, BLU, ref.blu.min, ref.blu.max);
	}

	G_put_map_row (out_fd, out_buf, row);
    }
    G_percent (row, nrows, 2);

    printf ("creating support files for %s\n", result);
    G_close_cell (out_fd);
}

int *
make_lookup(table, min, max, level, mult)
    unsigned char *table;
{
    int *lookup;
    int n;
    int x;
    int z;

    n = max - min + 1;
    lookup = (int *) G_calloc (n, sizeof(int));
    for (x=0; x < n; x++)
    {
	z = (int) table[x] * level / 255;
	if (z == level) z--;
	lookup[x] = z * mult;
    }
    return lookup;
}

paint (out, in, ncols, table, min, max)
    register CELL min, max;
    CELL *out, *in;
    int *table;
{
    register CELL v;

    while (ncols-- > 0)
    {
	v = *in++;
	if (v >= min && v <= max)
	    *out++ += table[v - min];
	else
	    out++;
    }
}
