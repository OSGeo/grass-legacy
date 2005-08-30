#include <math.h>
#include "gis.h"
#include "display.h"
#include "gprojects.h"

#include "local_proto.h"


int plot_grid (double grid_size, double east, double north)
{
	double x,y;
	double e1,e2;
	struct Cell_head window ;
	double row_dist, colm_dist;

	G_get_set_window (&window);

	/* pull right and bottom edges back one pixel; display lib bug? */
	row_dist = D_d_to_u_row(0) - D_d_to_u_row(1);
	colm_dist = D_d_to_u_col(1) - D_d_to_u_col(0);
	window.south = window.south + row_dist;
	window.east  = window.east  - colm_dist;

	G_setup_plot (
	    D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	    D_move_abs, D_cont_abs);

	/* Draw vertical grids */
	if (window.west > east)
		x = ceil((window.west - east)/grid_size) * grid_size + east ;
	else
		x = east - ceil((east - window.west)/grid_size) * grid_size ;

	while (x <= window.east)
	{
		G_plot_line (x, window.north, x, window.south);
		x += grid_size;
	}

	/* Draw horizontal grids
 *
 * For latlon, must draw in shorter sections
 * to make sure that each section of the grid
 * line is less than half way around the globe
 */
	e1 = (window.east*2 + window.west)/3;
	e2 = (window.west*2 + window.east)/3;

	if (window.south > north)
		y = ceil((window.south - north)/grid_size) * grid_size + north ;
	else
		y = north - ceil((north - window.south)/grid_size) * grid_size ;

	while (y <= window.north)
	{
		G_plot_line (window.east, y, e1, y);
		G_plot_line (e1, y, e2, y);
		G_plot_line (e2, y, window.west, y);
		y += grid_size;
	}

	return 0;
}


int plot_geogrid(double size, struct pj_info info_in, struct pj_info info_out)
{
double g;
double e1, e2, n1, n2;
double east, west, north, south;
double lat, lon;
int j, ll;
int SEGS=10;
struct Cell_head window ;

	/* geo current region */
	G_get_set_window (&window);

     /* get lat long min max */
     /* probably need something like boardwalk ?? */
     get_ll_bounds(&west, &east, &south, &north, window, info_in, info_out);

     /*
     fprintf(stderr, "REGION BOUNDS N=%f S=%f E=%f W=%f\n", 
		     north, south, east, west);
     */

     G_setup_plot (D_get_d_north(), D_get_d_south(),
		     D_get_d_west(), D_get_d_east(),
		     D_move_abs, D_cont_abs);

     
    /* Lines of Latitude */
    g = floor(north / size) * size ;
    e1 = east;
    for (j = 0; g >= south; j++, g -= size)
    {
        if (g == north || g == south) continue;
        for (ll = 0; ll < SEGS; ll++) {
                n1 = n2 = g;
                e1 = west + (ll *((east - west)/SEGS));
                e2 = e1 + ((east - west)/SEGS);
                        if (pj_do_proj(&e1, &n1, &info_in, &info_out) <0)
                                G_fatal_error("Error in pj_do_proj");
			check_coords(e1, n1, &lon, &lat, 1, window, info_in, info_out);
                        e1 = lon;
                        n1 = lat;
                        if (pj_do_proj(&e2, &n2, &info_in, &info_out) <0)
                                G_fatal_error("Error in pj_do_proj");
			check_coords(e2, n2, &lon, &lat, 1, window, info_in, info_out);
                        e2 = lon;
                        n2 = lat;
			G_plot_line(e1, n1, e2, n2);
	}
    }
    
    /* Lines of Longitude */
    g = floor (east / size) * size ;
    n1 = north;
    for (j = 0; g > west; j++, g -= size)
    {
        if (g == east || g == west) continue;
        for (ll = 0; ll < SEGS; ll++) {
                e1 = e2 = g;
                n1 = south + (ll *((north - south)/SEGS));
                n2 = n1 + ((north - south)/SEGS);
                        if (pj_do_proj(&e1, &n1, &info_in, &info_out) <0)
                        G_fatal_error("Error in pj_do_proj");
                        check_coords(e1, n1, &lon, &lat, 2, window, info_in, info_out);
                        e1 = lon;
                        n1 = lat;
                        if (pj_do_proj(&e2, &n2, &info_in, &info_out) <0)
                        G_fatal_error("Error in pj_do_proj");
                        check_coords(e2, n2, &lon, &lat, 2, window, info_in, info_out);
                        e2 = lon;
                        n2 = lat;
                        G_plot_line(e1, n1, e2, n2);
	}
    }

    return 0;
    
}

/******************************************************
 * initialze projection stuff and return proj structures
********************************************************/
void
init_proj(struct pj_info *info_in, struct pj_info *info_out)
{
double a, es;
struct Key_Value *in_proj_keys, *in_unit_keys;
struct Key_Value *out_proj_keys, *out_unit_keys;
char buff[100];


/* Proj stuff for geo grid */
/* Out Info */
out_proj_keys = G_get_projinfo();
out_unit_keys = G_get_projunits();
if (pj_get_kv(*&info_out,out_proj_keys,out_unit_keys) < 0)
        G_fatal_error("Can't get projection key values of current location");

/* In Info */
in_proj_keys = G_create_key_value();
in_unit_keys = G_create_key_value();

G_set_key_value("proj", "ll", in_proj_keys);
G_get_ellipsoid_parameters(&a, &es);
sprintf(buff, "%f", a);
G_set_key_value("a", buff, in_proj_keys);
sprintf(buff, "%f", es);
G_set_key_value("es", buff, in_proj_keys);

G_set_key_value("unit", "degree", in_unit_keys);
G_set_key_value("units", "degrees", in_unit_keys);
G_set_key_value("meters", "1.0", in_unit_keys);

if (pj_get_kv(*&info_in, in_proj_keys, in_unit_keys) < 0)
    G_fatal_error("Unable to set up lat/long projection parameters");

G_free_key_value( in_proj_keys);
G_free_key_value( in_unit_keys);
G_free_key_value( out_proj_keys);
G_free_key_value( out_unit_keys);

return;

}

/******************************************************
 * Use Proj to get min max bounds of region in lat long
********************************************************/
void
get_ll_bounds(
		double *w, 
		double *e, 
		double *s, 
		double *n, 
		struct Cell_head window,
		struct pj_info info_in,
		struct pj_info info_out)
{
    double east, west, north, south;
    double e1, w1, n1, s1;
    double ew, ns;
    double ew_res, ns_res;
    int first;

    *w = *e = *n = *s = -999.;
    west = east = north = south = -999.;

    e1 = window.east;
    w1 = window.west;
    n1 = window.north;
    s1 = window.south;

    /* calculate resolution based upon 100 rows/cols
     * to prevent possible time consuming parsing in large regions
     */
    ew_res = (e1-w1)/100.;
    ns_res = (n1-s1)/100.;

/* Get geographic min max from ala boardwalk style */
/* North */
first = 0;
for (ew = window.west; ew <= window.east; ew+=ew_res) {
        e1 = ew;
        n1 = window.north;
                if (pj_do_proj(&e1, &n1, &info_out, &info_in) <0)
                        G_fatal_error("Error in pj_do_proj");
                if (!first) {
                        north = n1;
                        first = 1;
                } else {
                if (n1 > north) north = n1;
                }
}
/*South*/
first = 0;
for (ew = window.west; ew <= window.east; ew+=ew_res) {
        e1 = ew;
        s1 = window.south;
                if (pj_do_proj(&e1, &s1, &info_out, &info_in) <0)
                        G_fatal_error("Error in pj_do_proj");
                if (!first) {
                        south = s1;
                        first = 1;
                } else {
                if (s1 < south) south = s1;
                }
}

/*East*/
first = 0;
for (ns = window.south; ns <= window.north; ns+=ns_res) {
        e1 = window.east;
        n1 = ns;
                if (pj_do_proj(&e1, &n1, &info_out, &info_in) <0)
                        G_fatal_error("Error in pj_do_proj");
                if (!first) {
                        east = e1;
                        first = 1;
                } else {
                if (e1 > east) east = e1;
                }
}

/*West*/
first = 0;
for (ns = window.south; ns <= window.north; ns+=ns_res) {
        w1 = window.west;
        n1 = ns;
                if (pj_do_proj(&w1, &n1, &info_out, &info_in) <0)
                        G_fatal_error("Error in pj_do_proj");
                if (!first) {
                        west = w1;
                        first = 1;
                } else {
                if (w1 < west) west = w1;
                }
}

*w = west;
*e = east;
*s = south;
*n = north;

return;
}

/******************************************************
 * check projected coords to make sure they do not 
 * go outside region -- if so re-project 
********************************************************/
void
check_coords(
		double e, 
		double n, 
		double *lon, 
		double *lat, 
		int par, 
		struct Cell_head w,
		struct pj_info info_in,
		struct pj_info info_out)
{
    double x, y;
    int proj = 0;

    *lat = y = n;
    *lon = x = e;

    if (e < w.west) {
                x = w.west;
                proj = 1;
    }
    if (e > w.east) {
                x = w.east;
                proj = 1;
    }
    if(n < w.south) {
                y = w.south;
                proj = 1;
    }
    if (n > w.north) {
                y = w.north;
                proj = 1;
    }

     if (proj) {
        /* convert original coords to ll */
        if (pj_do_proj(&e, &n, &info_out, &info_in) <0)
                G_fatal_error("Error in pj_do_proj1");

                if (par == 1) {
                /* lines of latitude -- const. northing */
                /* convert correct UTM to ll */
                if (pj_do_proj(&x, &y, &info_out, &info_in) <0)
                        G_fatal_error("Error in pj_do_proj2");

                /* convert new ll back to coords */
                if (pj_do_proj(&x, &n, &info_in, &info_out) <0)
                        G_fatal_error("Error in pj_do_proj3");
                *lat = n;
                *lon = x;
                }
                if (par == 2) {
                /* lines of longitude -- const. easting */
                /* convert correct UTM to ll */
                if (pj_do_proj(&x, &y, &info_out, &info_in) <0)
                        G_fatal_error("Error in pj_do_proj5");

                /* convert new ll back to coords */
                if (pj_do_proj(&e, &y, &info_in, &info_out) <0)
                        G_fatal_error("Error in pj_do_proj6");
                *lat = y;
                *lon = e;
                }
     }

return;
}
