/* functions that support interpolation in latitude-longitude projections */

#include "gis.h"
#include "main.h"

double	distance_LL ();
double  LL_set_geodesic_distance_lat (),
        set_sdlmr (),
        LL_geodesic_distance ();


/************************************************************************/
/*      This function initializes the search for nearest neighbors      */
/*      by locating the two data closest to the specified column in     */
/*      a linked list of row data                                       */

first_west_LL (ewptr, col)
EW              *ewptr;
SHORT           col;
{
    if (ewptr->start == NULL)           /* no data in this row */
        ewptr->walive = ewptr->ealive = FALSE;
    else if (ewptr->start == ewptr->start->prior) {	/* single datum */
	ewptr->west = ewptr->east = ewptr->start;
	ewptr->walive = FALSE;
	ewptr->ealive = TRUE;
	}
    else {	/* two or more data in this row */
        while (col > ewptr->start->x &&
	 ewptr->start->x < ewptr->start->next->x) /* start is west of col */
            ewptr->start = ewptr->start->next;
        ewptr->east = ewptr->start;
        ewptr->west = ewptr->start->prior;
	ewptr->walive = ewptr->ealive = TRUE;
        }    
}

 
double offset_distance_LL (offset)
SHORT   offset;
{
    extern double	*lat_diff;
       
    return (*(lat_diff + abs ((int) offset)));
}


/************************************************************************/
/*      Return TRUE if search is exhausted both west and east in a row  */
 
completed_row_LL (ewptr)
EW      *ewptr;
{
        return (!ewptr->walive && !ewptr->ealive);
}


find_neighbors_LL (ewptr, nbr_head, row, col, npoints, neighbors)
EW		*ewptr;
NEIGHBOR        *nbr_head;
int             npoints;
SHORT           row, col,
                *neighbors;
{
    extern double	*rowlook, *collook;
    MELEMENT    **Mptr;		/* double indirection !! */ 
    int 	westward = 1;       /* 1 if west of interpolation point */
    double      distance;
    short	*active;	/* TRUE if active search in this direction */

    active = &ewptr->walive;	/* TRUE if searching west in this row */
    Mptr = &ewptr->west;	/* process search west first, then east */
    do {
    	if (*active) {
	    distance = distance_LL (row, col, *Mptr);

            if (*neighbors < npoints) 
            	add_neighbor (Mptr, nbr_head, distance, ++(*neighbors));
            else if (!replace_neighbor (Mptr, nbr_head, distance)) 
            	*active = FALSE;       /* curtail search in this direction */
            
	    if (*active)
        	if (westward) 
		    extend_west (ewptr);
        	else
		    extend_east (ewptr);
            }
	
	active = &ewptr->ealive;
	Mptr = &ewptr->east;
	} while (westward--);	/* repeat loop for east and quit */
}


/************************************************************************/
/*      This function exhausts all possible nearest neighhbors          */
/*      within the row indexed by the ew search pointer                 */

exhaust_search_LL (ewptr, nbr_head, row, col)
EW              *ewptr;
NEIGHBOR        *nbr_head;
SHORT           row, col;
{
    double      distance;

    while (ewptr->walive) {             /* search active */
	distance = distance_LL (row, col, ewptr->west);

        if (!replace_neighbor (&ewptr->west, nbr_head, distance))
            break; /* curtail search in this direction */
        else
	    extend_west (ewptr);
        }

    while (ewptr->ealive) {		/* search active */               
	distance = distance_LL (row, col, ewptr->east);

        if (!replace_neighbor (&ewptr->east, nbr_head, distance))
            break; /* curtail search in this direction */
        else
	    extend_east (ewptr);
        }
}


extend_west (ewptr)
EW	*ewptr;
{
    if (ewptr->west->prior != ewptr->east)
        ewptr->west = ewptr->west->prior;
    else
        ewptr->walive = FALSE;
}


extend_east (ewptr)
EW	*ewptr;
{
    if (ewptr->east->next != ewptr->west)
	ewptr->east = ewptr->east->next;
    else
	ewptr->ealive = FALSE;
}


double distance_LL (row, col, Mptr)
SHORT		row, col;
MELEMENT	*Mptr;
{
    extern double       *rowlook, *collook;

    /* use lookup tables to increase distance calculation efficiency */
    LL_set_geodesic_distance (rowlook, row, Mptr->y);
    return (LL_geodesic_distance (*(collook + abs ((int) (col - Mptr->x)))));
}


/************************************************************************/
/*      Lookup tables storing pre-processed latitude and longitude data */
/*      are created for later use in selecting nearest neighbors        */

LL_lookup_tables (nrows, ncols)
SHORT   nrows, ncols;
{
    extern double               *rowlook, *collook, *lat_diff;
    extern struct Cell_head     window;
    double                      *nextrow, *nextcol, *next_diff,
				lon = 0.,
				lat = window.north - (0.5 * window.ns_res);
    SHORT                       i;

    nextrow = rowlook = (double *) G_calloc (nrows, sizeof (double));
    for (i = 0; i < nrows; i++, nextrow++, lat -= window.ns_res)
        *nextrow = LL_set_geodesic_distance_lat (lat);

    nextcol = collook = (double *) G_calloc (ncols, sizeof (double));
    for (i = 0; i < ncols; i++, nextcol++, lon += window.ew_res)
        *nextcol = set_sdlmr (lon);

    /* compute distance between latitudes at same longitude */
    next_diff = lat_diff = (double *) G_calloc (nrows, sizeof (double));
    for (i = 0; i < nrows; i++, next_diff++) {
	LL_set_geodesic_distance (rowlook, 0, i);
	*next_diff = LL_geodesic_distance (0.);
	}
}
