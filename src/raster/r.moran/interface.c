/***********************************************************************************/
/***                                 interface()                                 ***/
/*** 	    Function to get input from user and check files can be opened	 ***/
/***  										 ***/
/***  Jo Wood, Project ASSIST, Department of Geography, V1.2, 7th February 1992	 ***/
/***********************************************************************************/

#include "moran.h"

interface(argc,argv) 

    int	     argc;			/* Number of command line arguments.		*/
    char    *argv[];			/* Contents of command line arguments.		*/

{
    /*----------------------------------------------------------------------------------*/
    /*                                 INITIALISE					*/
    /*----------------------------------------------------------------------------------*/ 

    struct Option 	*rast_in;	/* This is a pointer to a structure holding	*/
					/* the all the text to describe each option.	*/
					/* It also stores the user's input.		*/

    struct Option	*rast_out;	/* Identical to *rast_in; same structure, diff-	*/
					/* erent contents. Also tol_val below.		*/

    struct Option	*win_size;	/* Size of local window.			*/
 
    struct Flag		*no_zero;	/* Include zeros in calculation ?		*/

    struct Flag		*brief;		/* Brief output only ?				*/

    G_gisinit (argv[0]);                /* This GRASS library function MUST be called   */
                                      	/* first to check for valid database and mapset.*/
                              		/* As usual argv[0] is the program name. This   */
                               		/* can be recalled using G_program_name().      */

    /*----------------------------------------------------------------------------------*/
    /*                              SET PARSER OPTIONS 					*/
    /*----------------------------------------------------------------------------------*/

    rast_in  = G_define_option();	/* Request pointer to memory for each option	*/
    rast_out = G_define_option();
    win_size = G_define_option();	

    no_zero  = G_define_flag();
    brief    = G_define_flag();

    /* Each option needs a 'key' (short description), a 'description` (a longer one)	*/
    /* a 'type' (eg intiger, or string), and an indication whether manditory or not	*/

    rast_in->key	  = "in";
    rast_in->description  = "Raster surface layer to measure";
    rast_in->type	  = TYPE_STRING;
    rast_in->required	  = YES;


    rast_out->key	  = "out";
    rast_out->description = "Output raster layer containing local Moran measures";
    rast_out->type	  = TYPE_STRING;
    rast_out->required	  = NO;
    rast_out->answer 	  = NULL;

    win_size->key	  = "size";
    win_size->description = "Size of processing window (odd number only)";
    win_size->type	  = TYPE_INTEGER;
    win_size->required	  = NO;
    win_size->answer	  = "3";

    no_zero->key	  = 'z';
    no_zero->description  = "Exclude zeros in calculation";

    brief->key		  = 'b';
    brief->description    = "Brief output only";

    if (G_parser(argc,argv))		/* Actually performs the prompting for 		*/
	exit(-1);			/* keyboard input. Returns a 0 if sucessful.	*/

    rast_in_name  = rast_in->answer;	/* Now that keyboard input has been parsed, we	*/
    rast_out_name = rast_out->answer;	/* can place the contents into our string arrays*/
    wsize	  = atoi(win_size->answer);
    NoZero	  = no_zero->answer;
    Brief	  = brief->answer;


    /*----------------------------------------------------------------------------------*/
    /*                          CHECK INPUT RASTER FILE EXISTS				*/
    /*----------------------------------------------------------------------------------*/


    if ((mapset_in=G_find_cell2(rast_in_name,""))==NULL)
    {
	char err[256];
	sprintf(err,"Raster map [%s] not available.",rast_in_name);
	G_fatal_error(err);
    }


    /*----------------------------------------------------------------------------------*/
    /*                       CHECK OUTPUT RASTER FILE DOES NOT EXIST			*/
    /*----------------------------------------------------------------------------------*/

    if (rast_out_name != NULL)
    {
    	mapset_out = G_mapset();	/* Set output to current mapset.		*/

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
    else
	wsize = 3;

    /*----------------------------------------------------------------------------------*/
    /*                    CHECK WINDOW SIZE IS NOT EVEN OR TOO LARGE			*/
    /*----------------------------------------------------------------------------------*/

    if ( (wsize%2 == 0) || (wsize > MAX_WSIZE)  || (wsize < 3) )
    {
    	char err[256];
    	sprintf(err,"ERROR: Inappropriate window size");
    	G_fatal_error(err);
    }
}
