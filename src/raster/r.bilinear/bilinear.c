#include "gis.h"
#include "bilinear.h"
#include <stdio.h>
#include <math.h>

int 	w_zero,		/* index of first non-null data column of output (W) */	
		e_zero;		/* index of last non-null data column of output (E) */

int		interp_cols;	/* number of non-null data columns per output row */

double	*in_easting,	/* cell center eastings of relevant input columns */
		*out_easting;	/* cell center eastings of non-null output columns */

double	*fractions;		/* distance fractions for each non-null output column;
						   values are used with each interpolation cycle */

int		*assoc;		/* index to input column west of output cell center */

int 	in_row;     /* last row read from input raster */

int		in_west_col;	/* input column west of first non-null output cell */

/* GPZ added function */
int irint ( a)
double a;
{
  return ((int) rint(a));
}



/* determine IF there will be null data on either west or east edge of the 
   output raster because it is outside the region of the input raster
*/
int out_of_range (coord, test)
double				coord;
int					test;
{
	double	easting,
			half_res = 0.5 * in_region.ew_res;

	/* provide default settings for non-null output data column start, end */
	if (test == EAST) 
		e_zero = out_cols - 1;
	else
		w_zero = 0;

	if (G_projection () == PROJECTION_LL) {
		if (in_region.west == in_region.east)	/* wrap-around */
			return (NO);
		coord = G_adjust_east_longitude (coord, in_region.west + half_res);
		easting = G_adjust_east_longitude (in_region.east - half_res,
		 in_region.west + half_res);
		if (coord > easting)
			return (YES);
		return (NO);
		}
	if (test == EAST) {		/* test EAST boundary */
		if (coord > in_region.east - half_res)
			return (YES);
		}
	else {
		if (coord < in_region.west + half_res)
		    return (YES); 
        }
	return (NO);
}
		

/* determine the number of output cells in each row, either west or east, that
   will be set to null data because these columns are outside the region of
   the input raster
*/
int null_columns (test)
int			test;
{
	double	easting;

	if (test == WEST) {
		easting = G_easting_to_col (in_region.west + (0.5 * in_region.ew_res),
	 	 &out_region);
		if ((int) easting + 0.5 < easting)
			w_zero = (int) easting + 1;
		else
			w_zero = (int) easting; 
		}
	else {
		easting = G_easting_to_col (in_region.east - (0.5 * in_region.ew_res),
	 	 &out_region);
		if ((int) easting + 0.5 > easting)
			e_zero = (int) easting - 1;
		else
			e_zero = (int) easting; 
		}
}


/* return array of eastings for columns slated for processing
*/
double *eastings_array (end, start, region)
int					end, start;
struct Cell_head    *region;
{
	double	*array;
	int		index = start;
	int		cols = region->cols;

	array = (double *) G_calloc (cols, sizeof (double));
	do {
		array[index] = G_col_to_easting (index + 0.5, region);
		} while (++index <= end);
	return (array);
}


/* create lookup arrays for eastings of cell centers of both input and 
	output rasters
*/
make_easting_lookup_arrays ()
{
	int		in_east_col;
	double	*temp, position;
	int 	index;

	interp_cols = e_zero - w_zero + 1;
    out_easting = eastings_array (e_zero, w_zero, &out_region);
    position = G_easting_to_col (out_easting[w_zero], &in_region);
	if ((int) position + 0.5 > position) {
		in_west_col = (int) position - 1;
		if (in_west_col < 0)
			in_west_col = in_cols - 1;
		}
	else
		in_west_col = (int) position; 
    position = G_easting_to_col (out_easting[e_zero], &in_region);
	if ((int) position + 0.5 < position) {
		in_east_col = (int) position + 1;
		if (in_east_col == in_cols)
            in_east_col = 0;
        }
	else
		in_east_col = (int) position; 

	/* account for possible LL wrap-around */

	if ((in_west_col == in_east_col) || ((in_west_col + 1) == in_east_col)) 
    	in_easting = eastings_array (in_region.cols - 1, 0, &in_region);
	else if (in_west_col < in_east_col)
		in_easting = eastings_array (in_east_col, in_west_col, &in_region);
	else {
		in_easting = eastings_array (in_east_col, 0, &in_region);
		temp = eastings_array (in_region.cols - 1, in_west_col, &in_region);
		index = in_west_col;
		while (index < in_region.cols)
			in_easting[index] = temp[index];
		free (temp);
		}
	}


/* return fractions of offset for each output column that will be 
   repeatedly applied by the binear interpolation formula
*/
set_fractions (incols, outcols)
int		incols, outcols;
{
	int		in = in_west_col, out = w_zero;
	double	*w, *e;

	fractions = (double *) G_calloc (outcols, sizeof (double));
	assoc = (int *) G_calloc (outcols, sizeof (int));
	w = in_easting + in;
	e = w + 1;

	if (G_projection () == PROJECTION_LL) {
		if (e == in_easting + incols)
			e = in_easting;
		*e = G_adjust_east_longitude (*e, *w);
		out_easting[out] = G_adjust_east_longitude (out_easting[out], *w);
		}

	while (1) {
		fractions[out] = (out_easting[out] - *w) / (*e - *w);
		assoc[out] = in;
		++out;
		if (out > e_zero)
			break;
		if (G_projection () == PROJECTION_LL) 
			out_easting[out] = G_adjust_east_longitude (out_easting[out], *w);
		while (out_easting[out] > *e) {
			w = e;
			e = w + 1;
			in++; 
			if (G_projection () == PROJECTION_LL) {
				if (e == in_easting + incols)
					e = in_easting;
				if (in == incols)
					in = 0;
				*e = G_adjust_east_longitude (*e, *w);
				}
			}
		}
}


/* initialize northing counters, load one row of input
    if null rows at N boundary, write to output here
*/
 
pre_bilinear_initialization (scell, outcell)
CELL				*scell, *outcell;
{
	int		i;
 
    in_north = G_row_to_northing (0.5, &in_region);
    out_north = G_row_to_northing (0.5, &out_region);
    in_row = out_row = 0;
    if (out_north > in_north) {
        if (G_projection () == PROJECTION_LL && npole_set) {
            i = 0;
            while (i < out_region.cols)
                scell[i++] = npole;
            in_row = -1;    /* next in_row read is row 0 !! */
            in_north = 90.;
            }
        else {  /* fill in null data rows */
            G_zero_cell_buf (outcell);
            do {
                if (G_put_map_row (outfd, outcell) < 0)
					exit (1);
                out_row++;
                out_north = G_row_to_northing (out_row + 0.5, &out_region);
                } while (out_north > in_north);
            if (G_get_map_row_nomask (infd, scell, 0) < 0)
                exit (1);
            }
        }    
    else {
        in_row = G_northing_to_row (out_north, &in_region);
        if (G_get_map_row_nomask (infd, scell, in_row) < 0)
            exit (1);
        in_north = G_row_to_northing (in_row + 0.5, &in_region);
        }
    in_south = in_north;
}

/*	load one or two input rows to "sandwich" the next output row in 
	preparation for interpolation of the row
*/
load_rows (ncell, scell)
CELL				**ncell, **scell;
{
	int		row = in_row;
	CELL	*temp;

	/* swap buffers */
	temp = *ncell;
	*ncell = *scell;
	*scell = temp;
	in_north = in_south;
	
	do {	/* find next input row south of next output row */
		in_south = G_row_to_northing (++in_row + 0.5, &in_region);
		} while (in_south > out_north);

    if (G_get_map_row_nomask (infd, *scell, in_row) < 0)
        exit (1);
	if (in_row != row + 1) {
		if (G_get_map_row_nomask (infd, *ncell, in_row - 1) < 0)
            exit (1);
        in_north = G_row_to_northing (in_row - 0.5, &in_region);
		}
}
	

/* bilinear interpolation between two adjacent input rows;
	one or more rows of output generated per call
 */

int bilinear (ncell, scell, outcell, mask)
CELL	*ncell, *scell, *outcell, *mask;
{
	int		out,		/* index to next output cell */
			w, e;
	int		lastcol = in_cols - 1;
	double	tmp1, tmp2;
	double	ew_fract, ns_fract;

    do {
		/* write null data, if any, at W border */
		for (out = 0; out < w_zero; outcell[out++] = 0);	

		ns_fract = (in_north - out_north) / (in_north - in_south);
		
		/* use mask, if present, to constrain output */

		if (mask) {	/* swap active regions to correctly read the mask */
			G_set_window (&out_region);
			if (G_get_map_row (maskfd, mask, out_row) < 0)
				exit (1);
			G_set_window (&in_region);
			}

        while (out <= e_zero) {
			w = assoc[out];
			e = (w == lastcol) ? 0 : w + 1;
			do {
				if (mask && mask[out] == 0)
					outcell[out++] = 0;
				else {
					ew_fract = fractions[out];
					tmp1 = ew_fract * ncell[e] + (1 - ew_fract) * ncell[w];
					tmp2 = ew_fract * scell[e] + (1 - ew_fract) * scell[w];
			outcell[out++] =
			 irint (ns_fract * tmp2 + (1 - ns_fract) * tmp1);
					}
	            } while (out <= e_zero && assoc[out] == assoc[out - 1]);
			}
		for (out = e_zero + 1; out < out_cols; outcell[out++] = 0);	
        if (G_put_map_row (outfd, outcell) < 0)
            exit(1);
		if (++out_row > out_rows)
			return 1;
        out_north = G_row_to_northing (out_row + 0.5, &out_region);
        } while (out_north >= in_south);

	return 0;
}


fill_south (ncell, scell, outcell, mask)
CELL	*ncell, *scell, *outcell, *mask;
{
	int		i;
	CELL	*temp;
 
    if (G_projection () == PROJECTION_LL && spole_set) {

		/* final interpolations between last input row south and south pole */

        /* swap buffers -- no need to share this address swap with main.c */
        temp = ncell;
        ncell = scell;
        scell = temp;
        in_north = in_south;

        i = 0;
        while (i < out_region.cols)
            scell[i++] = spole;
        in_south = -90.;

		bilinear (ncell, scell, outcell, mask);
        }
    else {  /* fill in null data rows */
        /* G_zero_cell_buf (outcell); INVALID:  uses active region row size */
        do {
			for (i = 0; i < out_cols; outcell[i++] = 0);
            if (G_put_map_row (outfd, outcell) < 0)
				exit (1);
            out_row++;
            out_north = G_row_to_northing (out_row + 0.5, &out_region);
            } while (out_row < out_rows);
        }    
}
