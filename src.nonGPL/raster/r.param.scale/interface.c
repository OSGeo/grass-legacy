/*********************************************************************************/
/***                               interface()                                 ***/
/***       Function to get input from user and check files can be opened       ***/
/***  									       ***/
/***         Jo Wood, Department of Geography, V1.2, 7th February 1992         ***/
/*********************************************************************************/

#include "param.h"

interface(argc,argv) 

    int	     argc;			/* Number of command line arguments.	*/
    char    *argv[];			/* Contents of command line arguments.	*/

{
    /*--------------------------------------------------------------------------*/
    /*                                 INITIALISE				*/
    /*--------------------------------------------------------------------------*/ 

    struct Option 	*rast_in,	/* Name of input file from command line.*/
    			*rast_out,	/* Holds name of output file.		*/
    			*tol1_val,	/* Tolerance values for feature		*/
			*tol2_val,	/* detection (slope and curvature).	*/
			*win_size,	/* Size of side of local window.	*/ 
			*parameter,	/* Morphometric parameter to calculate.	*/
			*expon,		/* Inverse distance exponent for weight.*/
			*vert_sc;	/* Vertical scaling factor.		*/

    struct Flag		*constr;	/* Forces quadratic through the central	*/
					/* cell of local window if selected.	*/

    G_gisinit (argv[0]);                /* GRASS function which MUST be called	*/
                                      	/* first to check for valid database 	*/
					/* and mapset and prompt user for input.*/

    /*--------------------------------------------------------------------------*/
    /*                            SET PARSER OPTIONS 				*/
    /*--------------------------------------------------------------------------*/

    rast_in   = G_define_option();	/* Request memory for each option.	*/
    rast_out  = G_define_option();
    tol1_val  = G_define_option();	
    tol2_val  = G_define_option();	
    win_size  = G_define_option();	
    parameter = G_define_option();	
    expon     = G_define_option();	
    vert_sc   = G_define_option();	

    constr    = G_define_flag();

    /* Each option has a 'key' (short descriptn), a 'description` (longer one)	*/
    /* a 'type' (eg int, or string), and an indication whether manditory or not	*/

    rast_in->key	  = "in";
    rast_in->description  = "Raster surface layer to process";
    rast_in->type	  = TYPE_STRING;
    rast_in->required	  = YES;


    rast_out->key	  = "out";
    rast_out->description = "Output raster layer containing morphometric parameter";
    rast_out->type	  = TYPE_STRING;
    rast_out->required	  = YES;

    tol1_val->key	  = "s_tol";
    tol1_val->description = "Slope tolerance that defines a `flat' surface (degrees)";
    tol1_val->type	  = TYPE_DOUBLE;
    tol1_val->required	  = NO;
    tol1_val->answer	  = "1.0";

    tol2_val->key	  = "c_tol";
    tol2_val->description = "Curvature tolerance that defines `planar' surface";
    tol2_val->type	  = TYPE_DOUBLE;
    tol2_val->required	  = NO;
    tol2_val->answer	  = "1.0";

    win_size->key	  = "size";
    win_size->description = "Size of processing window (odd number only)";
    win_size->type	  = TYPE_INTEGER;
    win_size->required	  = NO;
    win_size->answer	  = "3";

    parameter->key	  = "param";
    parameter->description= "Morphometric parameter to calculate";
    parameter->type	  = TYPE_STRING;
    parameter->required	  = NO;
    parameter->options	  = "elev,slope,aspect,profc,planc,longc,crosc,minic,maxic,feature";
    parameter->answer  	  = "elev";

    expon->key	  	  = "exp";
    expon->description 	  = "Exponent for distance weighting (0.0-4.0)";
    expon->type	  	  = TYPE_DOUBLE;
    expon->required	  = NO;
    expon->answer	  = "0.0";

    vert_sc->key	  = "zscale";
    vert_sc->description  = "Vertical scaling factor";
    vert_sc->type	  = TYPE_DOUBLE;
    vert_sc->required	  = NO;
    vert_sc->answer	  = "1.0";

    constr->key		  = 'c';
    constr->description   = "Constrain model through central window cell";


    if (G_parser(argc,argv))		/* Actually performs the prompting for	*/
	exit(-1);			/* keyboard input. 			*/

    rast_in_name  = rast_in->answer;	/* Now  keyboard input has been parsed, */
    rast_out_name = rast_out->answer;	/* can place the contents into strings	*/
    wsize	  = atoi(win_size->answer);
    constrained   = constr->answer;
    sscanf(expon->answer,"%lf",&exponent);
    sscanf(vert_sc->answer,"%lf",&zscale);
    sscanf(tol1_val->answer,"%lf",&slope_tol);
    sscanf(tol2_val->answer,"%lf",&curve_tol);

    if ((exponent<0.0) || (exponent >4.0))
	exponent = 0.0;

    if (zscale == 0.0)
	zscale = 1;

    if (!strcmp(parameter->answer,"elev"))
	    mparam = ELEV;
    else
    	if (!strcmp(parameter->answer,"slope"))
	    mparam = SLOPE;
	else
	    if (!strcmp(parameter->answer,"aspect"))
	    	mparam = ASPECT;
	    else
		if (!strcmp(parameter->answer,"profc"))
	    	    mparam = PROFC;
		else
		    if (!strcmp(parameter->answer,"planc"))
	    		mparam = PLANC;
		    else
			if (!strcmp(parameter->answer,"crosc"))
	    		    mparam = CROSC;
			else
			    if (!strcmp(parameter->answer,"longc"))
	    			mparam = LONGC;
			    else
			    	if (!strcmp(parameter->answer,"maxic"))
	    			    mparam = MAXIC;
			    	else
			    	    if (!strcmp(parameter->answer,"minic"))
	    			    	mparam = MINIC;
				    else
			    	    	if (!strcmp(parameter->answer,"feature"))
	    			    	    mparam = FEATURE;
			    	    else
			    	    {
	    				G_warning("Morphometric parameter not recognised. Assuming `Elevation'");
	    				mparam  = ELEV;
    			    	    }

    /*--------------------------------------------------------------------------*/
    /*                      CHECK INPUT RASTER FILE EXISTS			*/
    /*--------------------------------------------------------------------------*/


    if ((mapset_in=G_find_cell2(rast_in_name,""))==NULL)
    {
	char err[256];
	sprintf(err,"Raster map [%s] not available.",rast_in_name);
	G_fatal_error(err);
    }


    /*--------------------------------------------------------------------------*/
    /*                  CHECK OUTPUT RASTER FILE DOES NOT EXIST			*/
    /*--------------------------------------------------------------------------*/

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

    /*--------------------------------------------------------------------------*/
    /*                 CHECK WINDOW SIZE IS NOT EVEN OR TOO LARGE		*/
    /*--------------------------------------------------------------------------*/

    if ( (wsize/2 != (wsize-1)/2) || (wsize > MAX_WSIZE) )
    {
    	char err[256];
    	sprintf(err,"Inappropriate window size (too big or even)");
    	G_fatal_error(err);
    }

}
