/* %W% %G% */

/*
 *  Tocell is one of three methods for creating a grid-cell map
 *  showing distances from desired categories in an existing
 *  grid-cell map.    
 * 
 *  EXECUTION:   tocell <mapname>
 * 
 *  PREPARATION:
 *  <mapname> is a grid-cell map in which the distance categories shall 
 *  calculated.  This map must already contain zeros (0) in all cells
 *  for which distances need be calculated, a one in cells containing 
 *  categories from which distances shall be calculated, and a two in
 *  cells that are interior to areas (from which distances are calculated).
 *
 *  In addition, a "dist_in" file (defined below) must exist and be available
 *  which defines 1) Number of rows in file, 2) number of cols in file,
 *  and the distances (in cells) the user desires.
 *
 *  FLOW:
 *  1) Read the distance categories from the category file.
 *  2) Read entire file into core.
 *  3) Scan the file, keeping track of all cell positions containing non-
 *     zero values.
 *  4) For each cell:
 *     Search around that cell in increasingly larger boxes (shells).  
 *     When a cell containing a one (1) is located calculate
 *     the squared distance to that cell (deltx**2 + delty**2).  Continue
 *     search until there is no possibility of finding another (one containing)
 *     cell (1) closer.  Take square root of squared value and write out.
 *     Backup one shell and continue for next cell.
 *
 */

#include   <stdio.h>
#include "gis.h"
#include "disfrm.h"

/***********************************************************/
        
int Mail;

main (argc, argv)
int argc ;
char *argv[] ; 
{
    int file ;
    FILE *dist_in ;
    long DISTANCE[POSDISTS] ;
    int i ;
    struct Range range;
    struct Colors color ;
    struct Cell_head window ;
    struct Categories cats ;
    extern dist_error() ;
    int ncats;

    G_gisinit("TOCELL") ;

    G_set_error_routine(dist_error) ;

    if (argc < 2)
        G_fatal_error("Usage: tocell mapname") ;

    Mail = 1;
    if (argc == 3)
	if (strcmp (argv[2], "nomail") == 0)
	    Mail = 0;

    set_error_msg(argv[1]) ;
		
    if (G_get_cellhd(argv[1], G_mapset(), &window) == -1)
	G_fatal_error("Cell header not available\n") ;
		
    G_set_window(&window) ;     /* run analysis on map as is */

    if (G_read_cats(argv[1], G_mapset(), &cats) == -1)
	G_fatal_error("Cell categories not available\n") ;

    if (strncmp (cats.title, "Pre-Proximity Analysis", 16))
	G_fatal_error("Inappropriate file for tocell to work on") ;

    ncats = cats.num;
    for (i=0; i<ncats; i++)
    {
	sscanf(G_get_cat ((CELL)(i+1), &cats), "%d", &DISTANCE[i]) ;
    /* square these values so square roots need not be taken later */
	DISTANCE[i] = DISTANCE[i] * DISTANCE[i] ;
    }

    tocell (argv[1], window.rows, window.cols, DISTANCE, ncats) ;

/* Get categories again.  By this time, G_close_cell() has updated them */
    for (i=0; i<ncats; i++)
	sscanf(G_get_cat ((CELL)(i+1), &cats), "%d", &DISTANCE[i]) ;

    G_free_cats (&cats);
    ncats = G_number_of_cats (argv[1], G_mapset());

    update_cats(window.ns_res, &cats, ncats, DISTANCE) ;

    if (G_write_cats(argv[1], &cats) == -1)
	G_fatal_error("Cell categories not writeable\n") ;

/* make the color support file */
    G_read_range (argv[1], G_mapset(), &range);
    G_make_grey_scale (&color, 
	range.nmin?range.nmin:range.pmin,
	range.pmax?range.pmax:range.nmax);
    G_write_colors(argv[1], G_mapset(), &color) ;

    dist_error("Distance analysis complete", 0) ;
}
