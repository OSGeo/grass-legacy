/*****************************************************************************/
/***                                                                       ***/
/***                             write_vector()                            ***/
/***   	               Writes out to GRASS vector files.  		   ***/
/***                 Jo Wood, Project ASSIST, 30th May 1993.               ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"

write_vector(geom_type,attribute)
    int		geom_type,		/* Point, Line or Area			*/
		attribute;		/* Numerical attribute.			*/
{
    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE                                  */
    /*--------------------------------------------------------------------------*/ 
  
    FILE	*vect_fptr,		/* Points to one of the opened vect maps*/
		*vect_fptr_att;

    /*--------------------------------------------------------------------------*/
    /*                    FIND OUT WHICH VECTOR TO WRITE TO                     */
    /*--------------------------------------------------------------------------*/

    switch (V_featcode)			/* Each feature code written out to a	*/
    {					/*  separate vector map layer.		*/

	case 200:			/* Spot heights.			*/
	    if (O_spot == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".spot");
		fptr_spot = vect_info.dig_fp;
		fptr_spot_att = vect_info.att_fp;
		O_spot = TRUE;
	    }
	    vect_fptr = fptr_spot;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_spot_att;
	    break;

	case 201:			/* Contour lines.			*/
	    if (O_cont == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".cont");
		fptr_cont = vect_info.dig_fp;
		fptr_cont_att = vect_info.att_fp;
		O_cont = TRUE;
	    }
	    vect_fptr = fptr_cont;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_cont_att;
	    break;

	case 202:			/* Lake boundaries.			*/
	    if (O_lake == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".lake");
		fptr_lake = vect_info.dig_fp;
		fptr_lake_att = vect_info.att_fp;
		O_lake = TRUE;
	    }
	    vect_fptr = fptr_lake;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_lake_att;
	    break;

	case 203:			/* Break lines.				*/
	    if (O_break == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".break");
		fptr_break = vect_info.dig_fp;
		fptr_break_att = vect_info.att_fp;
		O_break = TRUE;
	    }
	    vect_fptr = fptr_break;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_break_att;
	    break;

	case 204:			/* Coastlines.				*/
	    if (O_coast == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".coast");
		fptr_coast = vect_info.dig_fp;
		fptr_coast_att = vect_info.att_fp;
		O_coast = TRUE;
	    }
	    vect_fptr = fptr_coast;	/* This is the file to write to.	*/
	    vect_fptr_att =fptr_coast_att;
	    break;

	case 205:			/* Ridge lines.				*/
	    if (O_temp == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".ridge");
		fptr_ridge = vect_info.dig_fp;
		fptr_ridge_att = vect_info.att_fp;
		O_temp = TRUE;
	    }
	    vect_fptr = fptr_ridge;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_ridge_att;

	    break;

	case 207:			/* Form lines.				*/
	    if (O_form == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector(".form");
		fptr_form = vect_info.dig_fp;
		fptr_form_att = vect_info.att_fp;
		O_form = TRUE;
	    }
	    vect_fptr = fptr_form;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_form_att;
	    break;

	default:
	    if (O_other == FALSE)	/* If false, haven't been opened yet.	*/
	    {
		open_vector("");
		fptr_other = vect_info.dig_fp;
		fptr_other_att = vect_info.att_fp;
		O_other = TRUE;
	    }
	    vect_fptr = fptr_other;	/* This is the file to write to.	*/
	    vect_fptr_att = fptr_other_att;
	    break;
    }

    /*--------------------------------------------------------------------------*/
    /*                   WRITE OUT X,Y COORDINATES AND ATTRIBUTES		*/
    /*--------------------------------------------------------------------------*/ 

    if (vect_fptr == NULL)
	return(0);

    vect_info.dig_fp = vect_fptr;
    vect_info.att_fp = vect_fptr_att;
    Vect_write_line(&vect_info,geom_type,points);

    if (geom_type == DOT)
    	write_att(vect_fptr_att, 'P', *(points->x), *(points->y), attribute);
    else
    	if (geom_type == LINE)
    	    write_att(vect_fptr_att, 'L', *(points->x+1), *(points->y+1), attribute);
	else    
	    if (geom_type == AREA)
    		write_att(vect_fptr_att, 'A', *(points->x), *(points->y), attribute);

}
