/**************************************************************************/
/*** 	                       profile.c				***/
/*** Functions to gather and fill oblique profiles across rasters.	***/
/*** V1.1 Modified to allow any profile angle, 31st July, 1995.		***/
/***									***/
/**************************************************************************/

#include "spline.h"

gather_profile(cont_ptr,profile_ptr,row,col,ncells_ptr,profile)
    CELL *cont_ptr,		/* Rasterized contour values.		*/
	 *profile_ptr;		/* Profile across DEM.			*/
    int	 row,col,		/* Location in DEM to extend profile.	*/
	 *ncells_ptr,		/* Number of cells in profile.		*/
	 profile;		/* Profile direction code.		*/
{
    int xoffset,		/* Displacements along profile direction*/
	yoffset,
	offset=0,
	x=col,			/* Position in profile.			*/
	y=row;


    /*------------------------------------------------------------------*/
    /*			CALCULATE PROFILE OFFSETS			*/
    /*------------------------------------------------------------------*/

    switch (profile)
    {
	case (1):
	    xoffset=1;		/* Horizontal profile direction.	*/
	    yoffset=0;
	    break;

	case (2):
	    xoffset=1;		/* NW to SE diagonal profile direction.	*/
	    yoffset=1;
	    break;

	case (3):
	    xoffset=0;		/* Vertical profile direction.	*/
	    yoffset=1;
	    break;

	case (4):
	    xoffset=1;		/* NE to SW diagonal profile direction.	*/
	    yoffset=-1;
	    break;

	default:
	    fprintf(stderr,"Error: Unknown profile direction\n");
	    exit(-1);
    }


				/* Move to start of profile.		*/
    while ((x>0) && (y>0) && (x<ncols-1) && (y<nrows-1))
    {
	x -= xoffset;
	y -= yoffset;
    }

				/* Fill in sparse profile.		*/
    while ((x+xoffset <= ncols) && (y+yoffset <= nrows) && 
	   (x+xoffset >= -1) && (y+yoffset >= -1))
    {
	*(profile_ptr + offset) = *(cont_ptr + y*ncols+x);
	offset++;
	x += xoffset;
	y += yoffset;
    }

    *ncells_ptr = offset;	
}

fill_profile(dem_ptr,weight_ptr,dem_prof,weight_prof,row,col,profile)
    CELL *dem_ptr;		/* Rasterized contour values.		*/
    int	 *weight_ptr;		/* interpolation weights.		*/
    CELL *dem_prof;		/* Profiles across DEM.			*/
    int  *weight_prof,
    	 row,col,		/* Location in DEM to extend profile.	*/
    	 profile;		/* Profile direction code.		*/
{

    int xoffset,		/* Displacements along profile direction*/
	yoffset,
	offset=0,		/* Profile offset.			*/

	x=col,			/* Position in profile.			*/
	y=row;

    switch (profile)
    {
	case (1):
	    xoffset=1;		/* Horizontal profile direction.	*/
	    yoffset=0;
	    break;

	case (2):
	    xoffset=1;		/* NW to SE diagonal profile direction.	*/
	    yoffset=1;
	    break;

	case (3):
	    xoffset=0;		/* Vertical profile direction.	*/
	    yoffset=1;
	    break;

	case (4):
	    xoffset=1;		/* NE to SW diagonal profile direction.	*/
	    yoffset=-1;
	    break;

	default:
	    fprintf(stderr,"Error: Unknown profile direction\n");
	    exit(-1);
    }


				/* Move to start of profile.		*/
    while ((x>0) && (y>0) && (x<ncols-1) && (y<nrows-1))
    {
	x -= xoffset;
	y -= yoffset;
    }

				/* Put interpolated profile in DEM	*/
    while ((x+xoffset <= ncols) && (y+yoffset <= nrows) && 
	   (x+xoffset >= -1) && (y+yoffset >= -1))
    {
	*(dem_ptr + y*ncols+x) = *(dem_prof + offset);
	*(weight_ptr + y*ncols+x) = *(weight_prof + offset);
	offset++;
	x += xoffset;
	y += yoffset;
    }
}

