/**********************************************************************************/
/***					interface()				***/
/***	 Parses input from keyboard and checks files exist/don't exist.		***/
/***     v1.2 23rd July, 1995.							***/
/***										***/
/**********************************************************************************/

#include "spline.h"

interface(argc,argv) 
    int argc;
    char *argv[];
{
    /*--------------------------------------------------------------------------*/
    /*			    	   INITIALISE					*/
    /*--------------------------------------------------------------------------*/ 

    struct Option 	*vect_in,	/* Structures required for G_parser()	*/
   	   		*rast_out,	
    			*interv;	/* Contour interval (for constrained 	*/
					/* spline fitting.			*/
    struct Flag		*rooks_case,	/* Select rooks case rasterising.	*/
	   		*simple_con;	/* Select simple spline consstraint.	*/

    G_gisinit (argv[0]);		/* Initialise GRASS module.		*/


    /*--------------------------------------------------------------------------*/
    /*				SET PARSER OPTIONS				*/
    /*--------------------------------------------------------------------------*/

    vect_in  = G_define_option();	/* Request pointer for each option.	*/
    rast_out = G_define_option();
    interv   = G_define_option();

    rooks_case= G_define_flag();
    simple_con= G_define_flag();

    vect_in->key		= "in";
    vect_in->description	= "vector contour map to be interpolated";
    vect_in->type		= TYPE_STRING;
    vect_in->required		= YES;


    rast_out->key		= "out";
    rast_out->description	= "Resultant Digital Elevation Model";
    rast_out->type		= TYPE_STRING;
    rast_out->required		= YES;

    interv->key			= "interval";
    interv->description 	= "Contour interval (or 0 for no interval constraint)";
    interv->type		= TYPE_INTEGER;
    interv->answer		= "0";

    rooks_case->key		= 'r';
    rooks_case->description	= "Rasterise contours using rooks case adjacency";

    simple_con->key		= 's';
    simple_con->description	= "Constrain interpolation using simple truncation";

    if (G_parser(argc,argv))
	exit(-1);		/*	Returns a 0 if sucessful		*/

    vect_in_name  = vect_in->answer;
    rast_out_name = rast_out->answer;
    interval      = atoi(interv->answer);
    truncation	  = simple_con->answer;
    rooks         = rooks_case->answer;

    /*--------------------------------------------------------------------------*/
    /*			   CHECK INPUT VECTOR FILE EXISTS			*/
    /*--------------------------------------------------------------------------*/

    if ((mapset_in=G_find_vector(vect_in_name,""))==NULL)
    {
	char err[256];
	sprintf(err,"Vector map [%s] not available.",vect_in_name);
	G_fatal_error(err);
    }

    /*--------------------------------------------------------------------------*/
    /*		   CHECK THE OUTPUT RASTER DOES NOT ALREADY EXIST		*/
    /*--------------------------------------------------------------------------*/

    mapset_out = G_mapset();		/* Set output to current mapset.	*/

    if (G_legal_filename(rast_out)==NULL)
    {
	char err[256];
	sprintf(err,"Illegal file name. Please try another.");
	G_fatal_error(err);
    }
    else
    {
	if (G_find_cell(rast_out_name,mapset_out) !=NULL)
	{
	    char err[256];
	    sprintf(err,"Raster map [%s] already exists.\nPlease try another.",
		rast_out_name);
	    G_fatal_error(err);
	}
    }
}
