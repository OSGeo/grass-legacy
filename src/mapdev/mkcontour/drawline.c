#include "contour.h"

/*
**  draw one line from two specified edges, linearly interpolating
**    to find the correct endpoints.  
**
**  Note that the order that the vertices are placed in the call to 
**    linterp is crutial for getting the correct interpolated value.
**    The line is interpolated from left to right or from top to bottom
**    rather than from min to max.
*/
cntr_drawline (contour, vert, hits, dfp, afp)
    int 		contour;
    int 		vert[5];
    int 		hits;
    FILE		*dfp; /* points to dig_file */		
    FILE		*afp; /* points to att_file */
{
    double		x_pt[2], y_pt[2]; /* variables sent to dig file */
    double		x,y; /* midpoints of line */
    int			type = LINE;
    int			num = 0;/* number of points in coordinate array */
    struct line_pnts 	line_coords; /* xy coordinates of a line for dig file*/

    /* write the contour line to the dig file */

    if (hits & CTOP) 
    {
	/*fprintf(stderr,"hits & CTOP\n");*/
	/*TL -> TR*/
	x_pt[num] = WEST + linterp(vert[0], contour, vert[1]) * EW_RES;
	y_pt[num] = NORTH;
	num++;
    }
    if (hits & CBOTTOM)
    {
	/*fprintf(stderr,"hits & CBOTTOM\n");*/

	/*BL -> BR*/
	x_pt[num] = WEST + (linterp(vert[3], contour, vert[2])) * EW_RES;
	y_pt[num] = SOUTH;
	num++;
    }
    if (hits & CLEFT)
    {
	/*fprintf(stderr,"hits & CLEFT\n");*/
	
	/*TL -> BL*/
	x_pt[num] = WEST;
	y_pt[num] = NORTH - linterp(vert[0], contour, vert[3]) * NS_RES;
	num++;
    }
    if (hits & CRIGHT)
    {
	/*fprintf(stderr,"hits & CRIGHT\n");*/
	/*TR -> BR*/
	x_pt[num] = EAST;
	y_pt[num] = NORTH - linterp(vert[1], contour, vert[2]) * NS_RES;
    }
    
    dig_Write_line(dfp,(char)type,x_pt,y_pt,2);

    /* fill in point structure */
    line_coords.x = x_pt;
    line_coords.y = y_pt;
    line_coords.n_points = num;

    /* calculate midpoint of line */    
    get_line_center (&x,&y, &line_coords);    
    write_att(afp,(char)type,x,y,contour);
}

/* 
** 
** rescale contour to a number between 0. and 1.
**  
*/
double
linterp (zmin, contour, zmax)
    int  contour, zmin, zmax;
{
    if (zmin == zmax)	/* div by zero.  should never get here */
	return (double) zmin;

    return ((double)(contour - zmin)) / ((double)(zmax - zmin));
}

