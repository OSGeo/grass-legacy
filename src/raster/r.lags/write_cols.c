/*****************************************************************************/
/***                                                                       ***/
/***                            write_cols()                               ***/
/***   	         Writes out colour file for lag maps			   ***/
/***               Jo Wood, Project ASSIST, 21st February 1995             ***/
/***                                                                       ***/
/*****************************************************************************/

#include "lags.h"


write_cols()
{
    /*------------------------------------------------------------------------*/
    /*                            INITIALISE                                  */
    /*------------------------------------------------------------------------*/ 

    struct Colors	colours;

    G_init_colors(&colours);

    /*------------------------------------------------------------------------*/
    /*                       FILL OUT COLORS STRUCTURE                        */
    /*------------------------------------------------------------------------*/ 
    switch(measure)
    {
	case MORAN:
	    
    	    G_add_color_rule(-1000,  0,  0,  0,			/* Black */
    			      -500,  0,  0,255, &colours);	/* Blue  */
    	    G_add_color_rule( -500,  0,  0,255,			/* Blue  */
    			         0,255,255,255, &colours);	/* White */
    	    G_add_color_rule(    0,255,255,255,			/* White */
    			       500,255,  0,  0, &colours);	/* Red   */
    	    G_add_color_rule(  500,255,  0,  0,			/* Red   */
    			      1000,   0, 0,  0, &colours);	/* Black */
	    break;
	

	default:
	    break;
    }


    /*------------------------------------------------------------------------*/
    /*                       WRITE OUT COLORS STRUCTURE                       */
    /*------------------------------------------------------------------------*/ 

    G_write_colors(rast_out1_name,mapset_out,&colours);

    G_free_colors(&colours);

}
