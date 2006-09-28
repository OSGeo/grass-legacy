#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/stats.h>


static const struct menu
{
	stat_func *method;	/* routine to compute new value */
	char *name;		/* method name */
	char *text;		/* menu display - full description */
} menu[] = {
	{c_ave,    "average",   "average (mean) value"},
	{c_median, "median",    "median value"},
	{c_mode,   "mode",      "most frequently occuring value"},
	{c_min,    "minimum",   "lowest value"},
	{c_max,    "maximum",   "highest value"},
	{c_quart1, "quart1",    "first quartile"},
	{c_quart3, "quart3",    "third quartile"},
	{c_perc90, "perc90",    "ninetieth percentile"},
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

int main( int argc, char *argv[])
{
	struct GModule *module;
	struct {
		struct Option *rastin, *rastout, *method;
	} parm;
	struct {
		struct Flag *nulls;
	} flag;
	int method;
	stat_func *method_fn;
	char *inmap;
	int infile, outfile;
	DCELL *outbuf;
	int row, col;
	struct Cell_head dst_w, src_w;
	int row_scale, col_scale;
	static DCELL **bufs;
	int *col_map, *row_map;
	static DCELL *values;

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

	if (G_parser(argc, argv))
		exit(EXIT_FAILURE);

	method = find_method(parm.method->answer);
	if (method < 0)
		G_fatal_error(_("Unknown method <%s>"), parm.method->answer);

	method_fn = menu[method].method;

	G_get_set_window(&dst_w);

	col_map = G_malloc((dst_w.cols + 1) * sizeof(int));
	row_map = G_malloc((dst_w.rows + 1) * sizeof(int));

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

	/* allocate values buffer */
	values = G_malloc(row_scale * col_scale * sizeof(DCELL));

	/* open new map */
	outfile = G_open_raster_new(parm.rastout->answer, DCELL_TYPE);
	if (outfile < 0)
		G_fatal_error(_("Not able to open cellfile for [%s]"), parm.rastout->answer);

	/* prevent complaints about window changes */
	G_suppress_warnings(1);

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

			if (null && flag.nulls->answer)
				G_set_d_null_value(&outbuf[col], 1);
			else
				(*method_fn)(&outbuf[col], values, n);
		}

		G_set_window(&dst_w);
		G_put_d_raster_row(outfile, outbuf);
	}

	G_percent(dst_w.rows, dst_w.rows, 2);

	G_close_cell(infile);
	G_close_cell(outfile);
    
	return(EXIT_SUCCESS);
}

