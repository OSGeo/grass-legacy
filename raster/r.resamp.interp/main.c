#include <stdlib.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/glocale.h>

static int neighbors;
static DCELL *bufs[4];
static int cur_row;

static void read_rows(int infile, int row)
{
	int end_row = cur_row + neighbors;
	int first_row = row;
	int last_row  = row + neighbors;
	int offset = first_row - cur_row;
	int keep = end_row - first_row;
	DCELL *tmp;
	int i;

	if (end_row >= last_row)
		return;

	if (keep > 0 && offset > 0)
		for (i = 0; i < keep; i++)
		{
			DCELL *tmp = bufs[i];
			bufs[i] = bufs[i + offset];
			bufs[i + offset] = tmp;
		}

	if (keep < 0)
		keep = 0;

	for (i = keep; i < neighbors; i++)
		G_get_d_raster_row(infile, bufs[i], first_row + i);

	cur_row = first_row;
}

int main( int argc, char *argv[])
{
	struct GModule *module;
	struct Option *rastin, *rastout, *method;
	char *inmap;
	int infile, outfile;
	DCELL *outbuf;
	int row, col;
	struct Cell_head dst_w, src_w;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		_("Resamples raster map layers using interpolation.");

	rastin  = G_define_standard_option(G_OPT_R_INPUT);
	rastout = G_define_standard_option(G_OPT_R_OUTPUT);

	method = G_define_option();
	method->key         = "method";
	method->type        = TYPE_STRING;
	method->required    = NO;
	method->description = _("Interpolation method");
	method->options     = "nearest,bilinear,bicubic";
	method->answer      = "bilinear";

	if (G_parser(argc, argv))
		exit(EXIT_FAILURE);

	if (G_strcasecmp(method->answer, "nearest") == 0)
		neighbors = 1;
	else if (G_strcasecmp(method->answer, "bilinear") == 0)
		neighbors = 2;
	else if (G_strcasecmp(method->answer, "bicubic") == 0)
		neighbors = 4;
	else
		G_fatal_error(_("Invalid method: %s"), method->answer);

	G_get_set_window(&dst_w);

	inmap = G_find_file2("cell", rastin->answer, "");
	if (!inmap)
		G_fatal_error(_("Couldn't find raster file %s"), rastin->answer);

	/* set window to old map */
	G_get_cellhd(rastin->answer, inmap, &src_w);

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

	/* allocate buffers for input rows */
	for (row = 0; row < neighbors; row++)
		bufs[row] = G_allocate_d_raster_buf();

	cur_row = -100;

	/* open old map */
	infile = G_open_cell_old(rastin->answer, inmap);
	if (infile < 0)
		G_fatal_error(_("Not able to open cellfile for [%s]"), rastin->answer);

	/* reset window to current region */
	G_set_window(&dst_w);
 
	outbuf = G_allocate_d_raster_buf();

	/* open new map */
	outfile = G_open_raster_new(rastout->answer, DCELL_TYPE);
	if (outfile < 0)
		G_fatal_error(_("Not able to open cellfile for [%s]"), rastout->answer);

	G_suppress_warnings(1);
	/* otherwise get complaints about window changes */

	switch (neighbors)
	{
	case 1:	/* nearest */
		for (row = 0; row < dst_w.rows; row++)
		{
			double north = G_row_to_northing(row + 0.5, &dst_w);
			double maprow_f = G_northing_to_row(north, &src_w) - 0.5;
			int maprow0 = (int) floor(maprow_f + 0.5);

			G_percent(row, dst_w.rows, 2);

			G_set_window(&src_w);
			read_rows(infile, maprow0);

			for (col = 0; col < dst_w.cols; col++)
			{
				double east = G_col_to_easting(col + 0.5, &dst_w);
				double mapcol_f = G_easting_to_col(east, &src_w) - 0.5;
				int mapcol0 = (int) floor(mapcol_f + 0.5);

				double c = bufs[0][mapcol0];

				if (G_is_d_null_value(&c))
				{
					G_set_d_null_value(&outbuf[col], 1);
				}
				else
				{
					outbuf[col] = c;
				}
			}

			G_set_window(&dst_w);
			G_put_d_raster_row(outfile, outbuf);
		}
		break;

	case 2:	/* bilinear */
		for (row = 0; row < dst_w.rows; row++)
		{
			double north = G_row_to_northing(row + 0.5, &dst_w);
			double maprow_f = G_northing_to_row(north, &src_w) - 0.5;
			int maprow0 = (int) floor(maprow_f);
			int maprow1 = maprow0 + 1;
			double v = maprow_f - maprow0;

			G_percent(row, dst_w.rows, 2);

			G_set_window(&src_w);
			read_rows(infile, maprow0);

			for (col = 0; col < dst_w.cols; col++)
			{
				double east = G_col_to_easting(col + 0.5, &dst_w);
				double mapcol_f = G_easting_to_col(east, &src_w) - 0.5;
				int mapcol0 = (int) floor(mapcol_f);
				int mapcol1 = mapcol0 + 1;
				double u = mapcol_f - mapcol0;

				double c00 = bufs[0][mapcol0];
				double c01 = bufs[0][mapcol1];
				double c10 = bufs[1][mapcol0];
				double c11 = bufs[1][mapcol1];

				if (G_is_d_null_value(&c00) ||
				    G_is_d_null_value(&c01) ||
				    G_is_d_null_value(&c10) ||
				    G_is_d_null_value(&c11))
				{
					G_set_d_null_value(&outbuf[col], 1);
				}
				else
				{
					DCELL c0 = u * (c01 - c00) + c00;
					DCELL c1 = u * (c11 - c10) + c10;
					DCELL c  = v * (c1  - c0 ) + c0 ;
					outbuf[col] = c;
				}
			}

			G_set_window(&dst_w);
			G_put_d_raster_row(outfile, outbuf);
		}
		break;

	case 4:	/* bicubic */
		for (row = 0; row < dst_w.rows; row++)
		{
			double north = G_row_to_northing(row + 0.5, &dst_w);
			double maprow_f = G_northing_to_row(north, &src_w) - 0.5;
			int maprow1 = (int) floor(maprow_f);
			int maprow0 = maprow1 - 1;
			int maprow2 = maprow1 + 1;
			int maprow3 = maprow1 + 2;
			double v = maprow_f - maprow1;

			G_percent(row, dst_w.rows, 2);

			G_set_window(&src_w);
			read_rows(infile, maprow0);

			for (col = 0; col < dst_w.cols; col++)
			{
				double east = G_col_to_easting(col + 0.5, &dst_w);
				double mapcol_f = G_easting_to_col(east, &src_w) - 0.5;
				int mapcol1 = (int) floor(mapcol_f);
				int mapcol0 = mapcol1 - 1;
				int mapcol2 = mapcol1 + 1;
				int mapcol3 = mapcol1 + 2;
				double u = mapcol_f - mapcol1;

				double c00 = bufs[0][mapcol0];
				double c01 = bufs[0][mapcol1];
				double c02 = bufs[0][mapcol2];
				double c03 = bufs[0][mapcol3];

				double c10 = bufs[1][mapcol0];
				double c11 = bufs[1][mapcol1];
				double c12 = bufs[1][mapcol2];
				double c13 = bufs[1][mapcol3];

				double c20 = bufs[2][mapcol0];
				double c21 = bufs[2][mapcol1];
				double c22 = bufs[2][mapcol2];
				double c23 = bufs[2][mapcol3];

				double c30 = bufs[3][mapcol0];
				double c31 = bufs[3][mapcol1];
				double c32 = bufs[3][mapcol2];
				double c33 = bufs[3][mapcol3];

				if (G_is_d_null_value(&c00) ||
				    G_is_d_null_value(&c01) ||
				    G_is_d_null_value(&c02) ||
				    G_is_d_null_value(&c03) ||

				    G_is_d_null_value(&c10) ||
				    G_is_d_null_value(&c11) ||
				    G_is_d_null_value(&c12) ||
				    G_is_d_null_value(&c13) ||

				    G_is_d_null_value(&c20) ||
				    G_is_d_null_value(&c21) ||
				    G_is_d_null_value(&c22) ||
				    G_is_d_null_value(&c23) ||

				    G_is_d_null_value(&c30) ||
				    G_is_d_null_value(&c31) ||
				    G_is_d_null_value(&c32) ||
				    G_is_d_null_value(&c33))
				{
					G_set_d_null_value(&outbuf[col], 1);
				}
				else
				{
					DCELL c0 = (u * (u * (u * (c03 - 3*c02 + 3*c01 - c00) + (-c03 + 4*c02  - 5*c01 + 2*c00)) + (c02 - c00)) + 2*c01)/2;
					DCELL c1 = (u * (u * (u * (c13 - 3*c12 + 3*c11 - c10) + (-c13 + 4*c12  - 5*c11 + 2*c10)) + (c12 - c10)) + 2*c11)/2;
					DCELL c2 = (u * (u * (u * (c23 - 3*c22 + 3*c21 - c20) + (-c23 + 4*c22  - 5*c21 + 2*c20)) + (c22 - c20)) + 2*c21)/2;
					DCELL c3 = (u * (u * (u * (c33 - 3*c32 + 3*c31 - c30) + (-c33 + 4*c32  - 5*c31 + 2*c30)) + (c32 - c30)) + 2*c31)/2;
					DCELL c  = (v * (v * (v * (c3  - 3*c2  + 3*c1  - c0 ) + (-c3  + 4*c2   - 5*c1  + 2*c0 )) + (c2  - c0 )) + 2*c1 )/2;
					outbuf[col] = c;
				}
			}

			G_set_window(&dst_w);
			G_put_d_raster_row(outfile, outbuf);
		}
		break;
	}

	G_percent(dst_w.rows, dst_w.rows, 2);

	G_close_cell(infile);
	G_close_cell(outfile);
    
	return(EXIT_SUCCESS);
}

