#include "gis.h"

/*
 * given a map window, and a box of N,S,E,W
 * does the box overlap the map window?
 * Returns 1 yes, 0 no.
 *
 * Note: knows about global wrap-around for lat-long
 */

G_window_overlap (window, N, S, E, W)
    struct Cell_head *window;
    double N, S, E, W;
{
    if (window->north <= S) return 0;
    if (window->south >= N) return 0;

    if (window->proj == PROJECTION_LL)
    {
	while (E < window->west)
	{
	    E += 360.0;
	    W += 360.0;
	}
	while (W > window->east)
	{
	    E -= 360.0;
	    W -= 360.0;
	}
    }

    if (window->east <= W) return 0;
    if (window->west >= E) return 0;

    return 1;
}

/*
 * This version returns the percentage (from 0 to 1) of the box 
 * contained in the window. This feature can be used during vector
 * plotting to decide if it is more efficient to do a level-one
 * read of the whole vector file, or to pay the price of a
 * level-two startup so only those arcs that enter the window are
 * actually read.
 *
 */

double
G_window_percentage_overlap (window, N, S, E, W)
    struct Cell_head *window;
    double N, S, E, W;
{
    double V,H;
    double n,s,e,w;
    double shift;

/* vertical height of the box that overlaps the window */
    if ((n = window->north) > N) n = N;
    if ((s = window->south) < S) s = S;
    V = n - s;
    if(V <= 0.0) return 0.0 ;

/* global wrap-around, part 1 */
    if (window->proj == PROJECTION_LL)
    {
	shift = 0.0;
	while (E+shift > window->east)
	    shift -= 360.0;
	while (E+shift < window->west)
	    shift += 360.0;
	E += shift;
	W += shift;
    }

/* horizontal width of the box that overlaps the window */
    if ((e = window->east) > E) e = E;
    if ((w = window->west) < W) w = W;
    H = e - w;
    if(H <= 0.0) return 0.0 ;

/* global wrap-around, part 2 */
    if (window->proj == PROJECTION_LL)
    {
	shift = 0.0;
	while (W+shift < window->west)
	    shift += 360.0;
	while (W+shift > window->east)
	    shift -= 360.0;
	if (shift)
	{
	    E += shift;
	    W += shift;
	    if ((e = window->east) > E) e = E;
	    if ((w = window->west) < W) w = W;
	    H += e - w;
	}
    }

    return (H*V)/((N-S)*(E-W));
}
