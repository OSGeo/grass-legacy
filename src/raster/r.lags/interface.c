/**************************************************************************/
/***                            interface()				***/
/***   Function to get input from user and check files can be opened	***/
/*** 							 		***/
/***	  Jo Wood, Department of Geography, V1.2, 7th February 1992	***/
/**************************************************************************/

#include "lags.h"

interface(argc,argv) 

    int	     argc;		/* Number of command line arguments.	*/
    char    *argv[];		/* Contents of command line arguments.	*/

{
    /*------------------------------------------------------------------*/
    /*				INITIALISE				*/
    /*------------------------------------------------------------------*/ 

    struct Option *rast_in,	/* Name of raster to measure.		*/
    		  *rast_out,	/* Name of raster holding lag map.	*/
    		  *meas;	/* Type of spatial dependence measure.	*/

    struct Flag	*nozero;	/* Exclude non-zero values from calcn.	*/
    struct Flag	*variog;	/* Calculate semi-varigram instead.	*/

    G_gisinit (argv[0]);	/* GRASS function which MUST be called	*/
                               	/* first to check for valid database 	*/
				/* and mapset and prompt user for input.*/

    /*------------------------------------------------------------------*/
    /*                      SET PARSER OPTIONS 				*/
    /*------------------------------------------------------------------*/

    rast_in	= G_define_option();  /* Request memory for options.	*/
    rast_out	= G_define_option();
    meas	= G_define_option();	

    nozero    = G_define_flag();
    variog    = G_define_flag();

    /* Each option has a 'key' (short descript), a 'description` (longer) */
    /* a 'type' (eg int, or string), and an indication whether
    /* manditory or not. */

    rast_in->key	  = "in";
    rast_in->description  = "Raster surface layer to measure";
    rast_in->type	  = TYPE_STRING;
    rast_in->required	  = YES;


    rast_out->key	  = "out";
    rast_out->description = "Output raster containing lag map";
    rast_out->type	  = TYPE_STRING;
    rast_out->required	  = YES;

    meas->key	  	  = "measure";
    meas->description 	  = "Spatial dependence measure (Moran's I or texture)";
    meas->type	  	  = TYPE_STRING;
    meas->options	  = "moran,texture";
    meas->required	  = NO;
    meas->answer	  = "moran";

    nozero->key		  = 'n';
    nozero->description   = "Calculate non-zero values only";

    variog->key		  = 'v';
    variog->description   = "Calculate semi-variogram instead";


    if (G_parser(argc,argv))	/* Actually performs the prompting for	*/
	exit(-1);		/* keyboard input. 			*/

    rast_in_name  = rast_in->answer; 	/* Store file names globally.	*/
    rast_out1_name = rast_out->answer;
    strcpy(rast_out2_name,rast_out->answer);
    strcat(rast_out2_name,".ns");

    if (strcmp(meas->answer,"texture")==0)
	measure=TEXTURAL;
    else
	measure=MORAN;

    vario = variog->answer;    			    	    

    /*------------------------------------------------------------------*/
    /*                  CHECK INPUT RASTER FILE EXISTS			*/
    /*------------------------------------------------------------------*/


    if ((mapset_in=G_find_cell2(rast_in_name,""))==NULL)
    {
	char err[256];
	sprintf(err,"Raster map [%s] not available.",rast_in_name);
	G_fatal_error(err);
    }


    /*------------------------------------------------------------------*/
    /*             CHECK OUTPUT RASTER FILE DOES NOT EXIST		*/
    /*------------------------------------------------------------------*/

    mapset_out = G_mapset();		/* Set output to current mapset.*/

    if (G_legal_filename(rast_out1_name)==NULL)
    {
        char err[256];
        sprintf(err,"Illegal file name. Please try another.");
        G_fatal_error(err);
    }
    else
    {
        if (G_find_cell2(rast_out1_name,mapset_out) !=NULL)
        {
            char err[256];
            sprintf(err,"Raster map [%s] exists.\nPlease try another\n",
		rast_out1_name);
            G_fatal_error(err);
        }

        if (G_find_cell2(rast_out2_name,mapset_out) !=NULL)
        {
            char err[256];
            sprintf(err,"Raster map [%s] exists.\nPlease try another\n",
		rast_out2_name);
            G_fatal_error(err);
        }
    }
}
