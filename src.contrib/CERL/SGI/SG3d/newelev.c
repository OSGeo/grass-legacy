
/*
**  Written by Bill Brown, Summer 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/


#include <gl.h>
#include <math.h>
#include "externs.h"

void
newelev ()
{
char name[200];


    if (NULL == G_ask_cell_old ("Choose new raster file for elevation:", name))
    {
	fprintf (stderr, "Command aborted\n");
	return ;
    }

    _newelev(name);

    fprintf (stderr, "Done.\n");

    /*
    do_clear ();
    do_fast_display();
    */
    do_display (Display_type, 0, 1);

    return ;
}




_newelev(name)
char *name;
{
char *elev_map;
char buff[200];
int row; 
long i, size;
FILEDESC elev_cell = NULL;

    elev_map = G_find_file2 ("cell", name, "");

    if ((elev_cell = G_open_cell_old(name, elev_map)) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", name);
	G_warning(buff);
	return(-1);
    }

    free(elev_buf);
    free(norm_buf);

    strcpy(Elevname, name);

    New_view = 1;

    size = X_Size * Y_Size;
    elev_buf = (int *)G_malloc (size * sizeof (int));
    norm_buf = (unsigned int*)G_malloc (size * sizeof(unsigned int));

/******************************************************************************/


    fprintf (stderr, "Initial load of data:  ");

    for (row = 0; row < Y_Size ; row++) 
	G_get_map_row(elev_cell, &(elev_buf[row*X_Size]), row) ; 

    G_close_cell(elev_cell);


    /* get Z range */
    Z_Min_real = Z_Max_real = elev_buf[0];
    for(i = 1; i < size; ++i){
	if(elev_buf[i] > Z_Max_real) Z_Max_real = elev_buf[i];
	else if(elev_buf[i] < Z_Min_real) Z_Min_real = elev_buf[i];
    }

    if(0 == Z_Min_real){
	Z_Min_notzero = 9999999.;  /* BIG */
	for(i = 1; i < size; ++i){
	    if(elev_buf[i] && elev_buf[i] < Z_Min_notzero)
		Z_Min_notzero = elev_buf[i];
	}
    }
	
    Z_Mid_real = (Z_Max_real + Z_Min_real) / 2.;

/*
    Zoff = Z_Min_real;
    offset to add to get real Z */

    /* subtract Zoff on next two lines & leave Zoff = previous surface
    Zoff so new surface is drawn in same "space" (necessary for animation
    or multiple surface viewing), otherwise Z scale would be reset - maybe
    make this happen when reset selected? */

    Z_Min = -Zoff;
    Z_Max = XYscale * (Z_Max_real - Z_Min_real) - Zoff;
    Z_Mid = (Z_Max + Z_Min) / 2.;
    Z_Span_real = Z_Span = (Z_Max_real - Z_Min_real);

    fprintf (stderr, "elevation range: %f to %f\n", Z_Min_real, Z_Max_real);
    fprintf (stderr, "Select \"Reset\" to recalculate new elevation range.");

/******************************************************************************/

    G_close_cell(name);

    recalc_normals(X_Modr, Y_Modr, elev_buf, norm_buf, Z_exag);

    return(0);
}




