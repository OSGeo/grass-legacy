#include "gis.h"

static struct Cell_head window;
static double square_meters;
static int projection;

static double units_to_meters_squared = 0.0;

/* these next are for lat-long only */
static int next_row;
static double north_value;
static double north;
static double (*darea0)();


/*!
 * \brief begin cell area calculations
 *
 * This routine must be called once before any call to
 * <i>G_area_of_cell_at_row.</i> It perform all inititalizations needed to do area calculations for
 * grid cells, based on the current window "projection" field. It can be used in either planimetric
 * projections or the latitude-longitude projection. It returns 
 *   2 if the projection is non-planimetric (i.e, latitude-longitude),
 *   1 if the projection is planimetric (i.e, UTM or SP) all cells are the same size, and 
 *   0 if the projection "projection" is not measurable (i.e, imagery or xy)
 * If the return value is 1 or 0, all the grid cells in the map have the same area. 
 * Otherwise the area of a grid cell varies with the row.
 *
 *  \param void
 *  \return int
 */

int G_begin_cell_area_calculations()
{
    double a, e2;
    double factor;

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
    default:
	square_meters = window.ns_res * window.ew_res;
	factor = G_database_units_to_meters_factor();
	if (factor > 0.0)
	    square_meters *= (factor * factor);
	return (factor > 0.0);
    }
}


/*!
 * \brief cell area in specified row
 *
 *  This routine returns the area in square meters of a cell in the
 * specified <b>row.</b> This value is constant for planimetric grids and
 * varies with the row if the projection is latitude-longitude.
 *
 *  \param row
 *  \return double
 */

double
G_area_of_cell_at_row ( register int row)
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


/*!
 * \brief begin polygon area calculations
 *
 *  This initializes the polygon area calculation routines. It is
 * used both for planimetric and latitude-longitude projections.
 * It returns 2 if the projection is latitude-longitude, 1 if the projection is
 * planimetric, and 0 if the projection doesn't hav e a metric (e.g. imagery.)
 *
 *  \param a
 *  \param e2
 *  \param factor
 *  \return int
 */

int G_begin_polygon_area_calculations()
{
    double a, e2;
    double factor;

    if ((projection = G_projection()) == PROJECTION_LL)
    {
	G_get_ellipsoid_parameters (&a, &e2);
	G_begin_ellipsoid_polygon_area (a, e2);
	return 2;
    }
    factor = G_database_units_to_meters_factor();
    if (factor > 0.0)
    {
	units_to_meters_squared = factor *factor;
	return 1;
    }
    units_to_meters_squared = 1.0;
    return 0;
}


/*!
 * \brief area in square meters of polygon
 *
 * Returns the area in square meters of the polygon
 * described by the <b>n</b> pairs of <b>x,y</b> coordinate vertices. It is
 * used both for planimetric and latitude-longitude projections.
 * <b>Note.</b> If the database is planimetric with the non-meter grid, this
 * routine performs the required unit conversion to produce square meters.
 * double <b>G_planimetric_polygon_area</b> (x, y, n) <i>area in
 * coordinate units</i> double *x, *y ; int n ;
 * Returns the area in coordinate units of the polygon described by the
 * <b>n</b> pairs of <b>x,y</b> coordinate vertices for planimetric grids.
 * If the units for <b>x,y</b> are meters, then the area is in square meters.
 * If the units are feet, then the area is in square feet, and so on.
 *
 *  \param x
 *  \param y
 *  \param n
 *  \return double
 */

double G_area_of_polygon(double *x,double *y,int n)
{
    double area;

    if (projection == PROJECTION_LL)
	area = G_ellipsoid_polygon_area(x,y,n);
    else
    	area = G_planimetric_polygon_area(x,y,n) * units_to_meters_squared;

    return area;
}
