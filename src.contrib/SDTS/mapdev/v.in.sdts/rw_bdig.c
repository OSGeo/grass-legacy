/*  @ (#)rw_bdig.c	2.1  6/26/87  */
/*  -dpg  */
#include <stdio.h>
#include "Vect.h"
#include "gis.h"

#ifdef OLD_LIB
#endif /*OLD_LIB*/

/* dig_write_line ()  write a line to bdig file.  */

dig_write_line (Map, Points, x, y, n_coors, type)
	struct Map_info *Map;
	struct line_pnts *Points;
    double *x ;
    double *y ;
    int n_coors;
    int type;
{

	/* old
    return (dig_Write_line (fp,(char) type, x, y, n_coors));
	*/

	/*newi with Vectlib*/
    if (0 > Vect_copy_xy_to_pnts (Points, x, y, n_coors))
        G_fatal_error ("Out of memory");

    Vect_write_line (Map, type, Points);

}

/* dig_write_point ()  create a digit Point line from a single x, y and write
*  it to bdig file.  */

dig_write_point (Map, Points, x, y, type)
    struct Map_info  *Map;
    struct line_pnts *Points;
    double *x ;
    double *y ;
    int type;
{

    int  n_coors ;
    double xa[2];
    double ya[2];

    n_coors = 2 ;
    xa[0] = xa[1] = *x;
    ya[0] = ya[1] = *y;
    /*old
    return (dig_Write_line (fp, (char) type, xa, ya, n_coors));
    */

	/*newi with Vectlib*/
    if (0 > Vect_copy_xy_to_pnts (Points, xa, ya, n_coors))
        G_fatal_error ("Out of memory");

    Vect_write_line (Map, type, Points);
}

/* breakout_xy() takes an array of x,y coordinates and  
*  these are used to avoid allocation of memory everytime  */
static  double *xarray ;
static  double *yarray ;
static  int  n_points = 0 ;

breakout_xy ( coors, n_coors, x, y)
    double *coors;
    int n_coors;
    double **x,  **y;
{
    register int fromcnt, tocnt;

    if ( n_points < n_coors )
    {
    	if (( xarray = (double *) G_malloc (n_coors * sizeof (double))) == NULL)
		G_fatal_error ("G_malloc failed\n");
    	if (( yarray = (double *) G_malloc (n_coors * sizeof (double))) == NULL)
    	{
		free (*xarray);
		G_fatal_error ("G_malloc failed\n");
    	}
	n_points = n_coors ;
    }


    /* load x and y from coors array */
    fromcnt = tocnt = 0;
    for (fromcnt = 0 ; fromcnt < 2*n_coors ; )
    {
	xarray[tocnt] = coors[fromcnt++];
	yarray[tocnt] = coors[fromcnt++];
	tocnt++;
    }

    /*  repoint the arg pointers  */
    *x = xarray ;
    *y = yarray ;

    return (0);
}

