
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include <gl.h>
#include <device.h>
#include <math.h>
#include "digit.h"

#include "externs.h"

static unsigned char *Red;
static unsigned char *Green;
static unsigned char *Blue;
static unsigned char *Set;

newcell ()
{
    char *vect_map;
    /*struct Cell_head wind;*/
    char name1[200];
    char name2[200];
    char name3[200];
    struct Range Range;
    int t, b, l, r;
    char *p;
    int dev;
    short val;
    /*float Z_Span, Z_Span2;*/

    float xfrom, yfrom, zfrom, xto, yto, zto;
    int save_three;

/******************************************************************************/



    save_three = Three_map;
/* Make sure maps are available */

    if (NULL == G_ask_cell_old ("Choose single cell file  OR  RED file:", name1))
    {
	Three_map = save_three;
	fprintf (stderr, "Command aborted\n");
	return  (0);
    }
    if (NULL == G_ask_cell_old ("Choose GREEN file or <RETURN> for single file", name2))
	Three_map = 0;	/* only single file */
    else
    {
	Three_map = 1;  /* RGB files */
	if (NULL == G_ask_cell_old ("Choose BLUE file or <RETURN> to abort", name3))
	{
	    Three_map = save_three;
	    fprintf (stderr, "Command aborted\n");
	    return  (0);
	}
    }

    if (Three_map)
	return _newcell (name1, name2, name3);
    else
	return _newcell (name1, NULL, NULL);
}


_newcell (name1, name2, name3)
    char *name1, *name2, *name3;
{
    register int row, col;
    int i;
    char *name1_map;
    char *name2_map;
    char *name3_map;
    FILEDESC cellfile1 = NULL;
    FILEDESC cellfile2 = NULL;
    FILEDESC cellfile3 = NULL;
    char buff[128];
    CELL *xarray;
    long *color_array;
    static int first = 1;

    {
	static int first = 1;

	if (first)
	{
	    /* alllocate buffers */
	    Red = G_malloc (X_Size);
	    Green = G_malloc (X_Size);
	    Blue = G_malloc (X_Size);
	    Set = G_malloc (X_Size);
	    visual = (int *)G_malloc (X_Size * Y_Size * sizeof (int));
	    first = 0;
	}
    }
    name1_map = G_find_file2 ("cell", name1, "");
    if ((cellfile1 = G_open_cell_old(name1, name1_map)) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", name1);
	G_fatal_error(buff);
    }
    if (Three_map)
    {
	name2_map = G_find_file2 ("cell", name2, "");
	if ((cellfile2 = G_open_cell_old(name2, name2_map)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name2);
	    G_fatal_error(buff);
	}
	name3_map = G_find_file2 ("cell", name3, "");
	if ((cellfile3 = G_open_cell_old(name3, name3_map)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name3);
	    G_fatal_error(buff);
	}
    }
    else
    {
	if (first)
	    first = 0;
	else
	    G_free_colors (&Pcolor);
	G_read_colors (name1, name1_map, &Pcolor);
    }

/* Allocate space for cell buffer */
    xarray = (int *)G_malloc (X_Size * sizeof (int));
    color_array = (long *)G_malloc (X_Size * sizeof (long));


/******************************************************************************/


    fprintf (stderr, "Loading data:  ");

    for (row = 0; row < Y_Size ; row++) 
    {
	int row_off;

	row_off = row * X_Size;

	if (Three_map)
	{
	    G_get_map_row (cellfile1, xarray, row); 
	    for (i = 0 ; i < X_Size ; i++)
		color_array[i] = xarray[i] & 0xff;

	    G_get_map_row (cellfile2, xarray, row); 
	    for (i = 0 ; i < X_Size ; i++)
		color_array[i] |= (xarray[i] & 0xff) << 8;
	
	    G_get_map_row (cellfile3, xarray, row); 

	    /* finish the composite color array */
	    for (i = 0 ; i < X_Size ; i++)
		visual[row_off+i] = color_array[i] | ((xarray[i]&0xff)<<16);
	}
	else
	{
	    G_get_map_row (cellfile1, xarray, row); 
	    G_lookup_colors (xarray,
		Red, Green, Blue, Set, X_Size, &Pcolor);
	    /* shift values down (or up) to start at 1 */
	    /* note will need to subtract 1 to index color arrays */
	    for (i = 0 ; i < X_Size ; i++)
	    {
		/*visual[row_off+i] = xarray[i];*/
		visual[row_off+i] = Red[i] & 0xff | 
		    ((Green[i] & 0xff) << 8) | ((Blue[i] & 0xff) << 16);
	    }
	}
    }

    /* color table has been converted into RGB map */
    Three_map = 1;

    G_close_cell(cellfile1);
    if (Three_map)
    {
	G_close_cell(cellfile2);
	G_close_cell(cellfile3);
    }
    free (xarray);
    free (color_array);

    fprintf (stderr, "Done.\n");

    do_clear ();
    return (1);
}
