/***************************************************************************
 * G_begin_cell_area_calculations ()
 *
 *    perform all inititalizations needed to do area calculations for
 *    grid cells, based on the current window "projection" field.
 * 
 *    returns: 0 if "projection" is not measurable (i.e, imagery or xy)
 *             1 if planimetric (i.e, UTM or SP) all cells are the same size,
 *               value from G_area_of_cell_at_row(row=0) good for all rows
 *             2 if non-planimetric (i.e, Lat-Long)
 *               must call G_area_of_cell_at_row() for each row
 *
 *
 * double
 * G_area_of_cell_at_row (row)
 *
 *    returns area of one cell in the specified row of the current window
 *    in square meters
 ***************************************************************************/

#include "gis.h"


static struct Cell_head window;
static double square_meters;
static int projection;

/*
 * the next factors handle state plane systems which have
 * coordinate grids in feet. 
 */
#define FEET_TO_METERS .3048
#define FEET_TO_METERS_SQUARE .09290304

/* these next are for lat-long only */
static int next_row;
static double north_value;
static double north;
static double (*darea0)();

extern double G_darea0_on_ellipsoid();
extern double G_darea0_on_sphere();

G_begin_cell_area_calculations()
{
    double a, e2;

    G_get_set_window(&window);
    switch(projection = window.proj)
    {
    case PROJECTION_LL:
	G_get_ellipsoid_parameters (&a, &e2);
	if (e2)
	{
	    G_begin_zone_area_on_ellipsoid (a, e2, window.ew_res/360.0);
	    darea0 = G_darea0_on_ellipsoid;
	}
	else
	{
	    G_begin_zone_area_on_sphere (a, window.ew_res/360.0);
	    darea0 = G_darea0_on_sphere;
	}
	next_row = 0;
	north_value = darea0 (north = window.north);
	return 2;
    case PROJECTION_UTM:
	square_meters = window.ns_res * window.ew_res;
	return 1;
    case PROJECTION_SP:
	square_meters = window.ns_res * window.ew_res * FEET_TO_METERS_SQUARE;
	return 1;
    default:          /* assume meters */
	square_meters = window.ns_res * window.ew_res;
	return window.proj != PROJECTION_XY;
    }
}

double
G_area_of_cell_at_row (row)
    register int row;
{
    register double south_value;
    register double cell_area;

    if (projection != PROJECTION_LL)
	return square_meters;

    if (row != next_row)
	north_value = darea0 (north = window.north - row * window.ns_res);

    south_value = darea0 (north -= window.ns_res);
    cell_area = north_value - south_value;

    next_row    = row+1;
    north_value = south_value;

    return cell_area;
}

G_begin_polygon_area_calculations()
{
    double a, e2;

    if ((projection = G_projection()) == PROJECTION_LL)
    {
	G_get_ellipsoid_parameters (&a, &e2);
	G_begin_ellipsoid_polygon_area (a, e2);
    }
    return projection != PROJECTION_XY;
}

double
G_area_of_polygon(x,y,n)
    double *x, *y;
{
    double area;
    double G_planimetric_polygon_area();
    double G_ellipsoid_polygon_area();

    if (projection == PROJECTION_LL)
	area = G_ellipsoid_polygon_area(x,y,n);
    else
    	area = G_planimetric_polygon_area(x,y,n);
    if (projection == PROJECTION_SP)
	area *= FEET_TO_METERS_SQUARE;
    
    return area;
}
