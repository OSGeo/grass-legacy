#include "contour.h"


cntr_do_contours (data,ncols,interval,dfp,afp,window)
    CELL	*data[2]; /* 2 rows of data from cell file */
    int		ncols; /* number of columns per row of data */ 
    int		interval;/*command line specified contour interval*/
    FILE	*dfp; /* pointer to dig file */
    FILE	*afp; /* pointer to attribute file */
    struct Cell_head	*window; /* info for active window */
{
    int		x; /* location in x direction */
    int		vert[5]; 
    int		i; /*looping variable */
    int		max,min; /* max and min values in vert[] */
    int		highest_contour, lowest_contour;/*bracket contours to be used */
    int		contour; /* current contour being calculated */
    int 	num ; /* the number of sides where crossing occurs */ 
    int 	hits ; /*keeps track of which side contour crosses cell */

    /* initialize the map location (x) to be used in coordinate calculations */
    EW_RES = window->ew_res;
    EAST = window->west + EW_RES/2;

    for (x=0; x <ncols-1; x++) /* assign data to vertex corners of square */
    {
	WEST = EAST;
	EAST = WEST + EW_RES;
	vert[0] = vert[4] = (int)data[0][x];	/* TL */
	vert[1] = (int)(data[0][x + 1]);	/* TR */
	vert[2] = (int)(data[1][x + 1]);	/* BR */
	vert[3] = (int)(data[1][x]);		/* BL */

	if(NOZEROES) /*if flag is on then ignore cells where zero data occurs*/
	    for (i=0; i< 4; i++)
	    {
		if (vert[i] ==0)
		    continue;
	    }

	/* find the min and max values in vert[] */
	max = min = vert[0];

	for (i = 1; i < 4; i++)
	{
	    if (vert[i] < min)
		min = vert[i];
	    else if (vert[i] > max)
		max = vert[i];
	}
	    

	/*calc possible contours for the grid_cell defined by verts[] */

	/* highest_contour_value */
	highest_contour=(max/interval)*interval;/*int math truncates remainder*/

	/* lowest_contour_value */
	if ((min % interval) > 0)
	    lowest_contour = (min/interval + 1) * interval;
	else
	    lowest_contour = (min/interval) * interval;

/*
** for each cell and each contour determine if and where any contour lines 
** occur and write them to the dig file
*/
	for(contour=lowest_contour;contour <= highest_contour;contour+=interval)
	{
	    /* check each cell side for crossing, recording which sides were
	    ** crossed and how many where crossed
	    */
	    num = 0;
	    hits = 0;
	/*DEBUG*/
	/*fprintf(stderr,"%d  %d  %d  %d \n",vert[0],vert[1],vert[2],vert[3]);*/
	    for (i = 0 ; i < 4 ; i++)
	    {
		if ((vert[i] <=  contour && contour <  vert[i+1]) ||
		    (vert[i] >   contour && contour >= vert[i+1]))
		{
		    hits |= (1 << i);	/* record which lines were crossed */
		    num++;
		}
	    }
	    
	    /*DEBUG*/
	    /*fprintf(stderr,"TOP %d  RIGHT %d  LEFT %d  BOTTOM %d \n",hits & CTOP, hits & CRIGHT, hits & CLEFT,hits & CBOTTOM);*/
	    if (!num || num & 0x01)	/* zero or odd */
		continue;

	    if (num == 2)	/* just draw line */
		cntr_drawline ( contour, vert, hits, dfp, afp);

	    else	/* must be 4 crossings */
	    {
		double sum, zmid;
		sum = 0;

		for (i = 0 ; i < 4 ; i++)/* bilinear interpolate for zmid */
		    sum += vert[i];
		zmid = sum / 4.;
		
		if (WITHIN (vert[0], contour, zmid) || 
		    WITHIN (zmid, contour, vert[0]))
		{ /* crosses diag */
		    cntr_drawline (contour, vert, TOPLEFT,dfp, afp);
		    cntr_drawline (contour, vert, BOTTOMRIGHT,dfp, afp);
		}
		else
		{
		    cntr_drawline (contour, vert, TOPRIGHT,dfp, afp);
		    cntr_drawline (contour, vert, BOTTOMLEFT,dfp, afp);
		}
	    }
 	}	
    }
}
