/* %W% %G% */

/*
 * TOCELL routine
 *                                                               
 * This routine is a "distance-determination" package.  A user    
 * chooses any set of categories attached to the layer (variable)
 * of interest.  tocell determines the distance between each     
 * cell and the nearest cell belonging to this chosen set of     
 * categories.                                                   
 *
 * COMPUTATION:
 *                                                               
 *  1) Reads original data base into an array.
 *  2) This is turned into a "binary map".  Non-zero cells are assigned
 *     the value "1".  All others remain "0".
 *  3) Cell by cell, the following is done:
 *     Search in ever expanding "shells" until a cell containing a "1".
 *       is found.  Calculate distance to this cell.  Continue searching
 *       until there are no unsearched cells which might contain a closer
 *       cell containing a "1".
 *     Write that distance out to desired file.
 *
 */

#include <stdio.h>
#include <math.h>
#include "gis.h"

#define LARGE 2000000000
#define YES 1
#define NO 0

/* Note:
 * MAPTYPE mut be able to handle the number of distance zones requested
 */
#define MAPTYPE unsigned char

static MAPTYPE *MAP ;

static int dist ;
static int shell_found, at_shell, first_shell ;
FILE *outfile, *fopen() ;

tocell (name, ROWS, COLS, dists, ndists)
    char *name ;
    int ROWS, COLS ;
    int dists[] ;
    int ndists ;
{
    int file ;
    static int counter = 0 ;
    int ino   ;
    int ixhi  ; int iyhi  ;
    int ixhim ; int iyhim ;
    int ixlo  ; int iylo  ;
    int ixlop ; int iylop ;
    int ixs   ; int iys   ;
    int i     ; int j     ; int k     ;
    int reset ;
    int int_dist ;
    int array_loc ;
    int adist ;
    int found ;
    int ix, iy, cur_x_pos, cur_y_pos ;
    register int ixe, iye ;
    CELL *cellbuf, *cellptr ;
    register MAPTYPE *mapptr ;

    int possible_dis  ;

    cellbuf = G_allocate_cell_buf() ;

/* Read in the map of interest ***************************************/
    if ( (file = G_open_cell_old(name, G_mapset()) ) < 0)
	G_fatal_error("Cell file not available\n") ;

    MAP = (MAPTYPE *)G_calloc(ROWS*COLS,sizeof(MAPTYPE)) ;
    if (!MAP)
	G_fatal_error("Insufficient memory for map") ;

/* copy each row read into the MAP */
    mapptr = MAP;
    for (j = 0 ; j < ROWS ; j++)
    {
	G_get_map_row(file, cellptr = cellbuf, j) ;

	for (k = 0; k < COLS; k++)
	    *mapptr++ = *cellptr++;
    }
    G_close_cell(file) ;

/* Reopen map for writing */
    if ( (file = G_open_cell_new(name) ) < 0)
	G_fatal_error("Cell file not available\n") ;

/*
* The actual distance determination section. Closest cells which
* may potentially be in the set from which distances are to be
* calculated are searched first.  Ever larger "shells" are
* subsequently searched.
*/

/* LOOP: Through ROWS of data base ***********************************/
    first_shell = 1 ;
    for(cur_y_pos = 0 ; cur_y_pos < ROWS ; cur_y_pos++)
    {
	at_shell = first_shell - 1 ;
	if (at_shell < 1)
		at_shell = 1 ;
	shell_found = 1 ;
/*
	fprintf(stderr, "%d ", cur_y_pos) ;
*/
	G_zero(cellbuf, COLS*sizeof(CELL)) ;

/* LOOP: Through COLS of data base *******************************/
	for(cur_x_pos = 0 ; cur_x_pos < COLS ; cur_x_pos++)
	{
	    /* If cell contains a category of interest, dist = 0; stop ***/
	    if(MAP[cur_y_pos*COLS+cur_x_pos])
		dist = 0 ;
	    /* otherwise, start the search *******************************/
	    else
	    {
		dist = LARGE ;
		while(1)
		{
		    ixlo = cur_x_pos - at_shell ;
		    ixhi = cur_x_pos + at_shell ;
		    iylo = cur_y_pos - at_shell ;
		    iyhi = cur_y_pos + at_shell ;
		    ixhim = ixhi - 1 ;
		    iyhim = iyhi - 1 ;
		    iylop = iylo + 1 ;
		    ixlop = ixlo + 1 ;

	    /* If no cells in "shell" within data base, stop *****/
		    if( (ixlo <     0) && (iylo <     0) &&
			(iyhi >= ROWS) && (ixhi >= COLS)    )
			    goto mark840 ;

	    /* Short circuit if we are past biggest zone area */
		    if( (at_shell * at_shell) > dists[ndists-1] )
		    {
			if (!dist)
			    dist = at_shell * at_shell ;
			goto mark840 ;
		    }

		    /* If no cell in "shell" possibly closer than current
		     *  distance, stop ***********************************/

		    possible_dis = at_shell * at_shell ;
		    if (dist <= possible_dis) 
			goto mark840 ;
    
		    /* Otherwise, start searching shell ******************/

		    /* Search side 1 of box : bottom *********************/
		    iy = iylo ;
		    if(iy < 0) ;
		    else
		    {
			ixs = ixlo  ;
			ixe = ixhim ;
			if(ixs < 0) ixs = 0 ;
			if(ixe >= COLS) ixe = COLS - 1;
			/* line(ixs, iy, ixe, iy) ; */
			mapptr = MAP + iy * COLS + ixs ;
			for(ix = ixs ; ix <= ixe ; ix++)
			    if(*mapptr++ == 1)
				check(ix, iy, cur_x_pos, cur_y_pos) ;
		    }
    
		    /* Search side 2 of box : right **********************/
		    ix = ixhi ;
		    if(ix >= COLS) ;
		    else
		    {
			iys = iylo  ;
			iye = iyhim ;
			if(iys < 0) iys = 0 ;
			if(iye >= ROWS) iye = ROWS - 1 ;
			/* line(ix, iys, ix, iye) ; */
			mapptr = MAP + iys * COLS + ix ;
			for(iy = iys ; iy <= iye ; iy++)
			{
			    if(*mapptr == 1)
				check(ix, iy, cur_x_pos, cur_y_pos) ;
			    mapptr += COLS ;
			}
		    }
    
		    /* Search side 3 of box : top ************************/
		    iy = iyhi ;
		    if(iy >= ROWS) ;
		    else
		    {
			ixs = ixlop  ;
			ixe = ixhi ;
			if(ixs < 0)     ixs = 0 ;
			if(ixe >= COLS) ixe = COLS - 1 ;
			/* line(ixs, iy, ixe, iy) ; */
			mapptr = MAP + iy * COLS + ixs ;
			for(ix = ixs ; ix <= ixe ; ix++)
			    if(*mapptr++ == 1)
				check(ix, iy, cur_x_pos, cur_y_pos) ;
		    }
    
		    /* Search side 4 of box : left ***********************/
		    ix = ixlo ;
		    if(ix < 0) ;
		    else
		    {
			iys = iylop  ;
			iye = iyhi ;
			if(iys < 0) iys = 0 ;
			if(iye >= ROWS) iye = ROWS - 1 ;
			/* line(ix, iys, ix, iye) ; */
			mapptr = MAP + iys * COLS + ix ;
			for(iy = iys ; iy <= iye ; iy++)
			{
			    if(*mapptr == 1)
				check(ix, iy, cur_x_pos, cur_y_pos) ;
			    mapptr += COLS ;
			}
		    }
		    at_shell++ ;
		}
	    }

mark840:
	    if (!dist)
		cellbuf[cur_x_pos] = 0 ;
	    else
	    {

	    /* Find where fits */
		found = 0 ;
		for (adist=0; adist<ndists; adist++)
		{
		    if (dist <= dists[adist])
		    {
			found = 1 ;
			break ;
		    }
		}

		if (found)
		    cellbuf[cur_x_pos] = adist+1 ;
		else
		    cellbuf[cur_x_pos] = ndists+1 ;
	    }

	    if (shell_found > 1)
		at_shell = shell_found - 1 ;
	    else
		at_shell = shell_found     ;

	    if (! cur_x_pos)
		first_shell = shell_found ;

	    shell_found = 1 ;
	}

/* Write out the new row */
	G_put_map_row(file, cellbuf) ;
    }

    G_close_cell(file) ;
}

check(ix, iy, cur_x_pos, cur_y_pos)
register int ix, iy, cur_x_pos, cur_y_pos ;
{
    register int newdist ;
    register int deltx, delty;

    deltx = cur_x_pos - ix ;
    delty = cur_y_pos - iy ;
    /* newdist= sqrt( deltx * deltx + delty * delty ) ; */
    newdist = deltx * deltx + delty * delty ;
    if (newdist > dist) 
	    return ;
    if (shell_found == 1) shell_found = at_shell ;
    dist = newdist ;
    return ;
}
