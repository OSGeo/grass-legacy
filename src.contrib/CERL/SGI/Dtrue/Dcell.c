/*  %W%  %G%  */

#include "gis.h"
#include "gl.h"

Dcell(name1, mapset1, name2, mapset2, name3, mapset3)
	char *name1, *mapset1 ;
	char *name2, *mapset2 ;
	char *name3, *mapset3 ;
{
	struct Cell_head wind ;
	struct Colors colors ;
	char window_name[64] ;
	char buff[128] ;
	int offset ;

	G_get_set_window (&wind) ;

	/*
	if (D_check_map_window(&wind))
		G_fatal_error("Setting map window") ;
		*/

	if (G_set_window(&wind) == -1) 
		G_fatal_error("Current window not settable") ;

/* Save the current map window with the graphics window */
	/*
	D_check_map_window(&wind) ;
	G_set_window (&wind);
	*/


/* Go draw the cell file */
	cell_draw(&wind, name1, mapset1, name2, mapset2, name3, mapset3) ;
}

static
cell_draw(wind1, name1, mapset1, name2, mapset2, name3, mapset3)
	struct Cell_head *wind1;
	char *name1, *mapset1;
	char *name2, *mapset2;
	char *name3, *mapset3;
{
	int i;
	char buff[128] ;
	int cellfile1 ;
	int cellfile2 ;
	int cellfile3 ;
	CELL *xarray ;
	long *color_array ;
	int cur_A_row ;
	int t, b, l, r ;
	long xsize, ysize;
	struct Cell_head wind;


/* open GL window */
	G_get_set_window (&wind) ;
	/*
	keepaspect ((wind.east - wind.west) / (wind.north - wind.south));
	*/
	prefsize ((long)((wind.east-wind.west)/wind.ew_res)+1, (long) ((wind.north-wind.south)/wind.ns_res)+10);

	winopen ("Dtrue");
	reshapeviewport ();
	RGBmode ();
	gconfig ();
	cpack (0);
	clear ();
	getsize (&xsize, &ysize);
	ortho2 (0., (float)xsize-1, (float)ysize-1, 0.);
	/*
	ortho2 (wind1->west, wind1->east, wind1->south, wind1->north);
	*/

	/* ortho2 (-1., 1., -1., 1.); */

/* Set up the screen, conversions, and graphics */
	wind.rows = ysize;
	wind.cols = xsize;
	wind.ns_res = (wind.north - wind.south) / ysize;
	wind.ew_res = (wind.east - wind.west) / xsize;
fprintf (stderr, "SIZE Y %d  X %d\n", ysize, xsize);
	/*
	G_adjust_window_to_box (&wind, &wind, ysize, xsize);
	*/
	if (G_set_window(&wind) == -1) 
		G_fatal_error("Current window not settable") ;


/* Make sure map is available */
	if ((cellfile1 = G_open_cell_old(name1, mapset1)) == -1) 
	{
		sprintf(buff,"Not able to open cellfile for [%s]", name1);
		G_fatal_error(buff) ;
	}
	if ((cellfile2 = G_open_cell_old(name2, mapset2)) == -1) 
	{
		sprintf(buff,"Not able to open cellfile for [%s]", name2);
		G_fatal_error(buff) ;
	}
	if ((cellfile3 = G_open_cell_old(name3, mapset3)) == -1) 
	{
		sprintf(buff,"Not able to open cellfile for [%s]", name3);
		G_fatal_error(buff) ;
	}

/* Allocate space for cell buffer */
	xarray = G_allocate_cell_buf() ; 
	color_array = (long *)G_malloc (xsize * sizeof (long));

/* loop for array rows */
	for (cur_A_row = 0; cur_A_row < ysize ; cur_A_row++)   /* RED */
	{
	    G_get_map_row(cellfile1, xarray, cur_A_row) ; 
	    for (i = 0 ; i < xsize ; i++)
		color_array[i] = xarray[i] & 0xff;

	    G_get_map_row(cellfile2, xarray, cur_A_row) ; 
	    for (i = 0 ; i < xsize ; i++)
		color_array[i] |= (xarray[i] & 0xff) << 8;
	
	    G_get_map_row(cellfile3, xarray, cur_A_row) ; 
	    /* and finally display it */
	    for (i = 0 ; i < xsize ; i++)
	    {
		color_array[i] |= (xarray[i] & 0xff) << 16;
		cpack (color_array[i]);
		pnt2i (i, cur_A_row);
	    }
	}

/* Wrap up and return */
	G_close_cell(cellfile1) ;
	G_close_cell(cellfile2) ;
	G_close_cell(cellfile3) ;
	free (xarray);
	free (color_array);
	return(0) ;

}
