/***********************************************************************************/
/***                                 interface()                                 ***/
/*** 	    Function to get input from user and check files can be opened	 ***/
/***  										 ***/
/***  Jo Wood, Project ASSIST, Department of Geography, V1.4, 18th July, 1995	 ***/
/***********************************************************************************/

#include "feature.h"

interface(argc,argv) 

    int	     argc;			/* Number of command line arguments.		*/
    char    *argv[];			/* Contents of command line arguments.		*/

{
    /*----------------------------------------------------------------------------------*/
    /*                                 INITIALISE					*/
    /*----------------------------------------------------------------------------------*/ 

    struct Option 	*dem_in,	/* Input Digital Elevation Model name.		*/
			*feat_in,	/* Input feature surface name.			*/
    			*rast_out;	/* Output thinned feature raster.		*/

    struct Flag		*create_sites,	/* Flag options placed in the Flag structure.	*/
	                *tri_corners;   /* Flag to indicate triangulation of corners.   */


    G_gisinit (argv[0]);                /* This GRASS library function MUST be called   */
                                      	/* first to check for valid database and mapset.*/
                              		/* As usual argv[0] is the program name. This   */
                               		/* can be recalled using G_program_name().      */

    /*----------------------------------------------------------------------------------*/
    /*                              SET PARSER OPTIONS 					*/
    /*----------------------------------------------------------------------------------*/

    feat_in  = G_define_option();
    dem_in   = G_define_option();	/* Request pointer to memory for each option	*/
    rast_out = G_define_option();
    create_sites = G_define_flag();
    tri_corners  = G_define_flag();

    /* Each option needs a 'key' (short description), a 'description` (a longer one)	*/
    /* a 'type' (eg intiger, or string), and an indication whether manditory or not	*/

    feat_in->key	  = "feat";
    feat_in->description  = "Raster surface feature layer from r.param.scale";
    feat_in->type	  = TYPE_STRING;
    feat_in->required	  = YES;

    dem_in->key		  = "dem";
    dem_in->description  = "DEM that matches feature classification";
    dem_in->type	  = TYPE_STRING;
    dem_in->required	  = YES;

    rast_out->key	  = "out";
    rast_out->description = "Output raster layer containing thinned surface features";
    rast_out->type	  = TYPE_STRING;
    rast_out->required	  = NO;

    create_sites->key	      = 's';
    create_sites->description = "Create sites file output";

    tri_corners->key          = 'c';
    tri_corners->description  = "Create corner values";

    if (G_parser(argc,argv))		/* Actually performs the prompting for 		*/
	exit(-1);			/* keyboard input. Returns a 0 if sucessful.	*/

    feat_in_name  = feat_in->answer;	/* Now that keyboard input has been parsed, we	*/
    dem_in_name   = dem_in->answer;	/* can place the contents into our string arrays*/
    rast_out_name = rast_out->answer;
    sites	  = create_sites->answer;
    corners       = tri_corners->answer;


    /*----------------------------------------------------------------------------------*/
    /*                          CHECK INPUT RASTER FILE EXISTS				*/
    /*----------------------------------------------------------------------------------*/


    if ((mapset1_in=G_find_cell2(feat_in_name,""))==NULL)
    {
	char err[256];
	sprintf(err,"Feature map [%s] not available.",feat_in_name);
	G_fatal_error(err);
    }

    if ((mapset2_in=G_find_cell2(dem_in_name,""))==NULL)
    {
	char err[256];
	sprintf(err,"DEM [%s] not available.",dem_in_name);
	G_fatal_error(err);
    }

    /*----------------------------------------------------------------------------------*/
    /*                       CHECK OUTPUT RASTER FILE DOES NOT EXIST			*/
    /*----------------------------------------------------------------------------------*/

    if(rast_out_name)
    {
    	mapset_out = G_mapset();		/* Set output to current mapset.	*/

        if (G_legal_filename(rast_out_name)==NULL)
        {
            char err[256];
            sprintf(err,"Illegal file name. Please try another.");
            G_fatal_error(err);
    	}
    	else
    	{
            if (G_find_cell2(rast_out_name,mapset_out) !=NULL)
            {
            	char err[256];
            	sprintf(err,"Raster map [%s] exists.\nPlease try another\n",rast_out_name);
            	G_fatal_error(err);
            }
    	}
    }
}
