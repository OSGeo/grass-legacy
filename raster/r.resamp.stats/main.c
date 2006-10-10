#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/stats.h>


static const struct menu
{
	stat_func *method;	/* routine to compute new value */
	stat_func_w *method_w;	/* routine to compute new value (weighted) */
	char *name;		/* method name */
	char *text;		/* menu display - full description */
} menu[] = {
	{c_ave,    w_ave,    "average",   "average (mean) value"},
	{c_median, w_median, "median",    "median value"},
	{c_mode,   w_mode,   "mode",      "most frequently occuring value"},
	{c_min,    NULL,     "minimum",   "lowest value"},
	{c_max,    NULL,     "maximum",   "highest value"},
	{c_quart1, w_quart1, "quart1",    "first quartile"},
	{c_quart3, w_quart3, "quart3",    "third quartile"},
	{c_perc90, w_perc90, "perc90",    "ninetieth percentile"},
	{c_sum,    w_sum,    "sum",       "sum of values"},
	{NULL,     NULL,        NULL}
};

static char *build_method_list(void)
{
	char *buf = G_malloc(1024);
	char *p = buf;
	int i;

	for (i = 0; menu[i].name; i++)
	{
		char *q;
		if (i)
			*p++ = ',';
		for (q = menu[i].name; *q; p++, q++)
			*p = *q;
	}
	*p = '\0';

	return buf;
}

static int find_method(const char *name)
{
	int i;

	for (i = 0; menu[i].name; i++)
		if (strcmp(menu[i].name, name) == 0)
			return i;

	return -1;
}

static int nulls;
static int infile, outfile;
static struct Cell_head dst_w, src_w;
static DCELL *outbuf;
static DCELL **bufs;
static int method;
static int row_scale, col_scale;

static void resamp_unweighted(void)
{
	stat_func *method_fn;
	DCELL *values;
	int *col_map, *row_map;
	int row, col;

	method_fn = menu[method].method;

	values = G_malloc(row_scale * col_scale * sizeof(DCELL));

	col_map = G_malloc((dst_w.cols + 1) * sizeof(int));
	row_map = G_malloc((dst_w.rows + 1) * sizeof(int));

	for (col = 0; col <= dst_w.cols; col++)
	{
		double x = G_col_to_easting(col, &dst_w);
		col_map[col] = (int) floor(G_easting_to_col(x + 0.5, &src_w));
	}

	for (row = 0; row <= dst_w.rows; row++)
	{
		double y = G_row_to_northing(row, &dst_w);
		row_map[row] = (int) floor(G_northing_to_row(y + 0.5, &src_w));
	}

	for (row = 0; row < dst_w.rows; row++)
	{
		int maprow0 = row_map[row + 0];
		int maprow1 = row_map[row + 1];
		int count = maprow1 - maprow0;
		int i;

		G_percent(row, dst_w.rows, 2);

		G_set_window(&src_w);

		for (i = 0; i < count; i++)
			G_get_d_raster_row(infile, bufs[i], maprow0 + i);

		for (col = 0; col < dst_w.cols; col++)
		{
			int mapcol0 = col_map[col + 0];
			int mapcol1 = col_map[col + 1];
			int null = 0;
			int n = 0;
			int i, j;

			for (i = maprow0; i < maprow1; i++)
				for (j = mapcol0; j < mapcol1; j++)
				{
					DCELL *src = &bufs[i - maprow0][j];
					DCELL *dst = &values[n++];

					if (G_is_d_null_value(src))
					{
						G_set_d_null_value(dst, 1);
						null = 1;
					}
					else
						*dst = *src;
				}

			if (null && nulls)
				G_set_d_null_value(&outbuf[col], 1);
			else
				(*method_fn)(&outbuf[col], values, n);
		}

		G_set_window(&dst_w);
		G_put_d_raster_row(outfile, outbuf);
	}
}

static void resamp_weighted(void)
{
	stat_func_w *method_fn;
	DCELL (*values)[2];
	double *col_map, *row_map;
	int row, col;

	method_fn = menu[method].method_w;

	values = G_malloc(row_scale * col_scale * 2 * sizeof(DCELL));

	col_map = G_malloc((dst_w.cols + 1) * sizeof(double));
	row_map = G_malloc((dst_w.rows + 1) * sizeof(double));

	for (col = 0; col <= dst_w.cols; col++)
	{
		double x = G_col_to_easting(col, &dst_w);
		col_map[col] = G_easting_to_col(x, &src_w);
	}

	for (row = 0; row <= dst_w.rows; row++)
	{
		double y = G_row_to_northing(row, &dst_w);
		row_map[row] = G_northing_to_row(y, &src_w);
	}

	for (row = 0; row < dst_w.rows; row++)
	{
		double y0 = row_map[row + 0];
		double y1 = row_map[row + 1];
		int maprow0 = (int) floor(y0);
		int maprow1 = (int) ceil(y1);
		int count = maprow1 - maprow0;
		int i;

		G_percent(row, dst_w.rows, 2);

		G_set_window(&src_w);

		for (i = 0; i < count; i++)
			G_get_d_raster_row(infile, bufs[i], maprow0 + i);

		for (col = 0; col < dst_w.cols; col++)
		{
			double x0 = col_map[col + 0];
			double x1 = col_map[col + 1];
			int mapcol0 = (int) floor(x0);
			int mapcol1 = (int) ceil(x1);
			int null = 0;
			int n = 0;
			int i, j;

			for (i = maprow0; i < maprow1; i++)
			{
				double ky = (i == maprow0) ? 1 - (y0 - maprow0)
					: (i == maprow1 - 1) ? 1 - (maprow1 - y1)
					: 1;

				for (j = mapcol0; j < mapcol1; j++)
				{
					double kx = (j == mapcol0) ? 1 - (x0 - mapcol0)
						: (j == mapcol1 - 1) ? 1 - (mapcol1 - x1)
						: 1;

					DCELL *src = &bufs[i - maprow0][j];
					DCELL *dst = &values[n++][0];

					if (G_is_d_null_value(src))
					{
						G_set_d_null_value(&dst[0], 1);
						null = 1;
					}
					else
					{
						dst[0] = *src;
						dst[1] = kx * ky;
					}
				}
			}

			if (null && nulls)
				G_set_d_null_value(&outbuf[col], 1);
			else
				(*method_fn)(&outbuf[col], values, n);
		}

		G_set_window(&dst_w);
		G_put_d_raster_row(outfile, outbuf);
	}
}

int main( int argc, char *argv[])
{
	struct GModule *module;
	struct {
		struct Option *rastin, *rastout, *method;
	} parm;
	struct {
		struct Flag *nulls, *weight;
	} flag;
	char *inmap;
	int row;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->keywords = _("raster");
	module->description =
		_("Resamples raster map layers using aggregation.");

	parm.rastin  = G_define_standard_option(G_OPT_R_INPUT);

	parm.rastout = G_define_standard_option(G_OPT_R_OUTPUT);

	parm.method = G_define_option();
	parm.method->key         = "method";
	parm.method->type        = TYPE_STRING;
	parm.method->required    = NO;
	parm.method->description = _("Aggregation method");
	parm.method->options     = build_method_list();
	parm.method->answer      = "average";

	flag.nulls = G_define_flag();
	flag.nulls->key = 'n';
	flag.nulls->description = _("Propagate NULLs");

	flag.weight = G_define_flag();
	flag.weight->key = 'w';
	flag.weight->description = _("Weight according to area (slower)");

	if (G_parser(argc, argv))
		exit(EXIT_FAILURE);

	nulls = flag.nulls->answer;

	method = find_method(parm.method->answer);
	if (method < 0)
		G_fatal_error(_("Unknown method <%s>"), parm.method->answer);

	G_get_set_window(&dst_w);

	inmap = G_find_file2("cell", parm.rastin->answer, "");
	if (!inmap)
		G_fatal_error(_("Couldn't find raster file %s"), parm.rastin->answer);

	/* set window to old map */
	G_get_cellhd(parm.rastin->answer, inmap, &src_w);

	/* enlarge source window */
	{
		double north = G_row_to_northing(0.5, &dst_w);
		double south = G_row_to_northing(dst_w.rows - 0.5, &dst_w);
		int r0 = (int) floor(G_northing_to_row(north, &src_w) - 0.5) - 1;
		int r1 = (int) floor(G_northing_to_row(south, &src_w) - 0.5) + 3;
		double west = G_col_to_easting(0.5, &dst_w);
		double east = G_col_to_easting(dst_w.cols - 0.5, &dst_w);
		int c0 = (int) floor(G_easting_to_col(west, &src_w) - 0.5) - 1;
		int c1 = (int) floor(G_easting_to_col(east, &src_w) - 0.5) + 3;

		src_w.south -= src_w.ns_res * (r1 - src_w.rows);
		src_w.north += src_w.ns_res * (-r0);
		src_w.west  -= src_w.ew_res * (-c0);
		src_w.east  += src_w.ew_res * (c1 - src_w.cols);
		src_w.rows  = r1 - r0;
		src_w.cols  = c1 - c0;
	}

	G_set_window(&src_w);

	row_scale = 1 + ceil(dst_w.ns_res / src_w.ns_res);
	col_scale = 1 + ceil(dst_w.ew_res / src_w.ew_res);

	/* allocate buffers for input rows */
	bufs = G_malloc(row_scale * sizeof(DCELL *));
	for (row = 0; row < row_scale; row++)
		bufs[row] = G_allocate_d_raster_buf();

	/* open old map */
	infile = G_open_cell_old(parm.rastin->answer, inmap);
	if (infile < 0)
		G_fatal_error(_("Not able to open cellfile for [%s]"), parm.rastin->answer);

	/* reset window to current region */
	G_set_window(&dst_w);

	/* allocate output buffer */
	outbuf = G_allocate_d_raster_buf();

	/* open new map */
	outfile = G_open_raster_new(parm.rastout->answer, DCELL_TYPE);
	if (outfile < 0)
		G_fatal_error(_("Not able to open cellfile for [%s]"), parm.rastout->answer);

	/* prevent complaints about window changes */
	G_suppress_warnings(1);

	if (flag.weight->answer && menu[method].method_w)
		resamp_weighted();
	else
		resamp_unweighted();

	G_percent(dst_w.rows, dst_w.rows, 2);

	G_close_cell(infile);
	G_close_cell(outfile);
    
	return(EXIT_SUCCESS);
}

