
#include <stdio.h>
#include <stdlib.h>

#include "gis.h"

int main(int argc, char **argv)
{
	struct GModule *module;
	struct {
		struct Option *in, *out, *old, *new;
	} opt;
	struct {
		struct Flag *d, *q;
	} flag;
	struct Colors colr;
	struct Categories cats;
	int verbose;
	int colrfile;
	int diagonal;
	char *in_name;
	char *out_name;
	int oldval;
	int newval;
	char *mapset;
	int in_fd;
	int out_fd;
	CELL *in_prev, *in_curr, *in_next;
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

	opt.old = G_define_option();
	opt.old->key         = "old";
	opt.old->type        = TYPE_INTEGER;
	opt.old->required    = NO;
	opt.old->description = "Value to write for input cells which are non-NULL";

	opt.new = G_define_option();
	opt.new->key         = "new";
	opt.new->type        = TYPE_INTEGER;
	opt.new->required    = NO;
	opt.new->description = "Value to write for \"grown\" cells";

	flag.d = G_define_flag();
	flag.d->key         = 'd';
	flag.d->description = "Grow diagonally";

	flag.q = G_define_flag();
	flag.q->key         = 'q';
	flag.q->description = "Quiet";

	if (G_parser(argc, argv))
		exit(1);

	in_name = opt.in->answer;
	out_name = opt.out->answer;

	if (opt.old->answer)
		oldval = atoi(opt.old->answer);

	if (opt.new->answer)
		newval = atoi(opt.new->answer);

	diagonal = flag.d->answer;
	verbose = !flag.q->answer;

	mapset = G_find_cell2(in_name, "");
	if (!mapset)
		G_fatal_error("input file [%s] not found", in_name);

	nrows = G_window_rows();
	ncols = G_window_cols();

	in_fd = G_open_cell_old(in_name, mapset);
	if (in_fd < 0)
		G_fatal_error("unable to open input file <%s@%s>", in_name, mapset);

	out_fd = G_open_cell_new(out_name);
	if (out_fd < 0)
		G_fatal_error("unable to open output file <%s>", out_name);

	if (G_read_cats(in_name,mapset,&cats) == -1)
	{
		G_fatal_error("error in reading cats file for %s", in_name);
		exit(1);
	}

	if (G_read_colors(in_name,mapset,&colr) == -1)
	{
		G_warning("error in reading colr file for %s", in_name);
		colrfile = 0;
	}
	else
		colrfile = 1;

	in_prev = G_allocate_cell_buf();
	in_curr = G_allocate_cell_buf();
	in_next = G_allocate_cell_buf();

	out_row = G_allocate_cell_buf();

	G_get_c_raster_row(in_fd, in_curr, 0);

	for (row = 0; row < nrows; row++)
	{
		CELL *tmp;

		if (row < nrows - 1)
			G_get_c_raster_row(in_fd, in_next, row + 1);

		for (col = 0; col < ncols; col++)
		{
			/*
			 *	6 2 7
			 *	3 1 4
			 *	8 5 9
			 */

			if (!G_is_c_null_value(&in_curr[col]))
			{
				out_row[col] = opt.old->answer
					? oldval
					: in_curr[col];
				continue;
			}

			if (row >= 1 &&
			    !G_is_c_null_value(&in_prev[col]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_prev[col];
				continue;
			}

			if (col >= 1 &&
			    !G_is_c_null_value(&in_curr[col - 1]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_curr[col - 1];
				continue;
			}

			if (col <= ncols - 2 &&
			    !G_is_c_null_value(&in_curr[col + 1]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_curr[col + 1];
				continue;
			}

			if (row <= nrows - 2 &&
			    !G_is_c_null_value(&in_next[col]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_next[col];
				continue;
			}

			if (diagonal && col >= 1 && row >= 1 &&
			    !G_is_c_null_value(&in_prev[col - 1]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_prev[col - 1];
				continue;
			}

			if (diagonal && col <= ncols - 2 && row >= 1 &&
			    !G_is_c_null_value(&in_prev[col + 1]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_prev[col + 1];
				continue;
			}

			if (diagonal && col >= 1 && row <= nrows - 2 &&
			    !G_is_c_null_value(&in_next[col - 1]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_next[col - 1];
				continue;
			}

			if (diagonal && col <= ncols - 2 && row <= nrows - 2 &&
			    !G_is_c_null_value(&in_next[col + 1]))
			{
				out_row[col] = opt.new->answer
					? newval
					: in_next[col + 1];
				continue;
			}

			G_set_c_null_value(&out_row[col], 1);
		}

		G_put_c_raster_row(out_fd, out_row);

		if (verbose)
			G_percent(row, nrows, 2);

		tmp = in_prev;
		in_prev = in_curr;
		in_curr = in_next;
		in_next = tmp;
	}

	if (verbose)
		G_percent(row, nrows, 2);

	G_close_cell(in_fd);
	G_close_cell(out_fd);

	if (G_write_cats(out_name,&cats) == -1)
		G_warning("error writing cats file for %s", out_name);

	if (colrfile)
		if (G_write_colors(out_name,mapset,&colr) == -1)
			G_warning("error writing colr file for %s", out_name);

	return 0;
}

