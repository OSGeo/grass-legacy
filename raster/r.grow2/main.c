
#include <stdio.h>
#include <stdlib.h>

#include "gis.h"

static int size;
static int count;
static int (*neighbors)[2];

static void setup_neighbors(double radius)
{
	int r2 = (int) (radius * radius);
	int n;
	int i, x, y;

	size = (int) radius;

	n = size * 2 + 1;

	neighbors = G_malloc(n * n * 2 * sizeof(int));

	count = 0;

	for (i = 1; i <= r2; i++)
	{
		for (y = 0; y < n; y++)
		{
			int dy = y - size;
			int dy2 = dy * dy;

			for (x = 0; x < n; x++)
			{
				int dx = x - size;
				int dx2 = dx * dx;

				if (dx2 + dy2 != i)
					continue;

				neighbors[count][0] = dx;
				neighbors[count][1] = dy;
				count++;
			}
		}
	}
}

int main(int argc, char **argv)
{
	struct GModule *module;
	struct {
		struct Option *in, *out, *rad, *old, *new;
	} opt;
	struct {
		struct Flag *q;
	} flag;
	struct Colors colr;
	struct Categories cats;
	int verbose;
	int colrfile;
	char *in_name;
	char *out_name;
	double radius;
	int oldval;
	int newval;
	char *mapset;
	int in_fd;
	int out_fd;
	CELL **in_rows;
	CELL *out_row;
	int nrows, row;
	int ncols, col;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Generates a raster map layer"
		"with contiguous areas grown by one cell.";

	opt.in = G_define_option();
	opt.in->key          = "input";
	opt.in->type         = TYPE_STRING;
	opt.in->required     = YES;
	opt.in->gisprompt    = "old,cell,raster";
	opt.in->description  = "Name of existing input raster file";

	opt.out = G_define_option();
	opt.out->key         = "output";
	opt.out->type        = TYPE_STRING;
	opt.out->required    = YES;
	opt.out->gisprompt   = "new,cell,raster";
	opt.out->description = "Name of output raster file";

	opt.rad = G_define_option();
	opt.rad->key         = "radius";
	opt.rad->type        = TYPE_DOUBLE;
	opt.rad->required    = NO;
	opt.rad->description = "Radius of buffer";
	opt.rad->answer      = "1.01";

	opt.old = G_define_option();
	opt.old->key         = "old";
	opt.old->type        = TYPE_INTEGER;
	opt.old->required    = NO;
	opt.old->description = "Value to write for input cells which are non-NULL (-1 => NULL)";

	opt.new = G_define_option();
	opt.new->key         = "new";
	opt.new->type        = TYPE_INTEGER;
	opt.new->required    = NO;
	opt.new->description = "Value to write for \"grown\" cells";

	flag.q = G_define_flag();
	flag.q->key         = 'q';
	flag.q->description = "Quiet";

	if (G_parser(argc, argv))
		exit(1);

	in_name = opt.in->answer;
	out_name = opt.out->answer;

	radius = atof(opt.rad->answer);

	if (opt.old->answer)
		oldval = atoi(opt.old->answer);

	if (opt.new->answer)
		newval = atoi(opt.new->answer);

	verbose = !flag.q->answer;

	mapset = G_find_cell2(in_name, "");
	if (!mapset)
		G_fatal_error("input file [%s] not found", in_name);

	nrows = G_window_rows();
	ncols = G_window_cols();

	setup_neighbors(radius);

	in_fd = G_open_cell_old(in_name, mapset);
	if (in_fd < 0)
		G_fatal_error("unable to open input file <%s@%s>", in_name, mapset);

	out_fd = G_open_cell_new(out_name);
	if (out_fd < 0)
		G_fatal_error("unable to open output file <%s>", out_name);

	if (G_read_cats(in_name, mapset, &cats) == -1)
	{
		G_fatal_error("error in reading cats file for %s", in_name);
		exit(1);
	}

	if (G_read_colors(in_name, mapset, &colr) == -1)
	{
		G_warning("error in reading colr file for %s", in_name);
		colrfile = 0;
	}
	else
		colrfile = 1;

	if (opt.old->answer && oldval >= 0)
		G_set_cat(oldval, "original cells", &cats);

	if (opt.new->answer)
		G_set_cat(newval, "grown cells", &cats);

	in_rows = G_malloc((size * 2 + 1) * sizeof(CELL *));

	for (row = 0; row < (size * 2 + 1); row++)
		in_rows[row] = G_allocate_cell_buf();

	out_row = G_allocate_cell_buf();

	for (row = 0; row < size + 1; row++)
		G_get_c_raster_row(in_fd, in_rows[size + row], row);

	for (row = 0; row < nrows; row++)
	{
		CELL *tmp;
		int i;

		for (col = 0; col < ncols; col++)
		{
			CELL *c = &in_rows[size][col];

			if (!G_is_c_null_value(c))
			{
				if (opt.old->answer)
				{
					if (oldval < 0)
						G_set_c_null_value(&out_row[col], 1);
					else
						out_row[col] = oldval;
				}
				else
					out_row[col] = *c;

				continue;
			}

			for (i = 0; i < count; i++)
			{
				int dx = neighbors[i][0];
				int dy = neighbors[i][1];
				int x = col + dx;
				int y = row + dy;

				if (x < 0 || x >= ncols || y < 0 || y >= nrows)
					continue;

				c = &in_rows[size + dy][x];

				if (!G_is_c_null_value(c))
				{
					out_row[col] = opt.new->answer
						? newval
						: *c;
					break;
				}
			}

			if (i == count)
				G_set_c_null_value(&out_row[col], 1);
		}

		G_put_c_raster_row(out_fd, out_row);

		if (verbose)
			G_percent(row, nrows, 2);

		if (row < nrows - 1)
			G_get_c_raster_row(in_fd, in_rows[0], row + 1);

		tmp = in_rows[0];
		for (i = 0; i < size * 2; i++)
			in_rows[i] = in_rows[i + 1];
		in_rows[size * 2] = tmp;
	}

	if (verbose)
		G_percent(row, nrows, 2);

	G_close_cell(in_fd);
	G_close_cell(out_fd);

	if (G_write_cats(out_name, &cats) == -1)
		G_warning("error writing cats file for %s", out_name);

	if (colrfile)
		if (G_write_colors(out_name, G_mapset(), &colr) == -1)
			G_warning("error writing colr file for %s", out_name);

	return 0;
}

