
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

/*
#include "gis.h"
#include <gl.h>
*/
#include <math.h>
#include "externs.h"

void do_clear();

static unsigned char *Red;
static unsigned char *Green;
static unsigned char *Blue;
static unsigned char *Set;

newcell ()
{
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
    int ret, save_three;

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
	ret =  _newcell (name1, name2, name3);
    else if (strcmp(name1, Elevname))
	ret =  _newcell (name1, NULL, NULL);
    else
	ret =  _newcell_is_elev(name1, NULL);

    if(ret > 0){   /* successful, so change globals */
	strcpy(Cellname[0], name1);
	strcpy(Cellname[1], name2);
	strcpy(Cellname[2], name3);
    }

    return(ret);
}

static int firstcell = 1;
static int colalloc = 0;

_newcell (name1, name2, name3)
    char *name1, *name2, *name3;
{
    register int row, i;
    char *name1_map;
    char *name2_map;
    char *name3_map;
    FILEDESC cellfile1 = NULL;
    FILEDESC cellfile2 = NULL;
    FILEDESC cellfile3 = NULL;
    char buff[128];
    CELL *xarray;
    long *color_array;

    {

	if (firstcell)
	{
	    /* alllocate buffers */
	    Red = G_malloc (X_Size);
	    Green = G_malloc (X_Size);
	    Blue = G_malloc (X_Size);
	    Set = G_malloc (X_Size);
	    visual = (int *)G_malloc (X_Size * Y_Size * sizeof (int));
	    firstcell = 0;
	}
    }
    name1_map = G_find_file2 ("cell", name1, "");
    if ((cellfile1 = G_open_cell_old(name1, name1_map)) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", name1);
	G_warning(buff);
	return(-1);
    }
    if (Three_map)
    {
	name2_map = G_find_file2 ("cell", name2, "");
	if ((cellfile2 = G_open_cell_old(name2, name2_map)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name2);
	    G_warning(buff);
	    return(-1);
	}
	name3_map = G_find_file2 ("cell", name3, "");
	if ((cellfile3 = G_open_cell_old(name3, name3_map)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name3);
	    G_warning(buff);
	    return(-1);
	}
    }
    else
    {
	if (colalloc)
	    G_free_colors (&Pcolor);
	G_read_colors (name1, name1_map, &Pcolor);
	colalloc = 1;
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

    G_close_cell(cellfile1);
    if (Three_map)
    {
	G_close_cell(cellfile2);
	G_close_cell(cellfile3);
    }
    free (xarray);
    free (color_array);

    fprintf (stderr, "Done.\n");

    /*
    do_clear ();
    */
    return (1);
}


_newcell_is_elev (name1, name1_map)
char *name1, *name1_map;
{
    CELL *xarray;
    long *color_array;
    register int row, i;
    char buff[128];
    
    if(name1_map == NULL)
	name1_map = G_find_file2 ("cell", name1, "");
    if (firstcell)
    {
	/* alllocate buffers */
	Red = G_malloc (X_Size);
	Green = G_malloc (X_Size);
	Blue = G_malloc (X_Size);
	Set = G_malloc (X_Size);
	visual = (int *)G_malloc (X_Size * Y_Size * sizeof (int));
	firstcell = 0;
    }
    if(colalloc)
	G_free_colors (&Pcolor);
    G_read_colors (name1, name1_map, &Pcolor);
    colalloc = 1;

    fprintf (stderr, "Loading data:  ");

#ifdef USE_SHORT
    xarray = (int *)G_malloc (X_Size * sizeof (int));
#endif
#ifdef USE_CHAR
    xarray = (int *)G_malloc (X_Size * sizeof (int));
#endif
    for (row = 0; row < Y_Size ; row++) 
    {
	int row_off;
	short *ts;
	unsigned char *tc;
	int *ti;

	row_off = row * X_Size;

#ifdef USE_SHORT
	ts = &(elev_buf[row_off]);
	ti = xarray;
	for(i =0; i < X_Size; i++)
	    *ti++ = *ts++;
#else
#ifdef USE_CHAR
	tc = &(elev_buf[row_off]);
	ti = xarray;
	for(i =0; i < X_Size; i++)
	    *ti++ = *tc++;
#else
	xarray = &elev_buf[row_off];
#endif
#endif

	G_lookup_colors (xarray,
	    Red, Green, Blue, Set, X_Size, &Pcolor);
	for (i = 0 ; i < X_Size ; i++)
	{
	    visual[row_off+i] = Red[i] & 0xff | 
		((Green[i] & 0xff) << 8) | ((Blue[i] & 0xff) << 16);
	}

    }
#ifdef USE_SHORT
    free(xarray);
#endif
#ifdef USE_CHAR
    free(xarray);
#endif

    /* color table has been converted into RGB map */
    Three_map = 0;

    fprintf (stderr, "Done.\n");

    return (1);
}
