/***********************************************************************************/
/***                                 interface()                                 ***/
/*** 	    Function to get input from user and check files can be opened	 ***/
/***  										 ***/
/***  Jo Wood, Project ASSIST, Department of Geography, V1.1, 24th January 1992	 ***/
/***********************************************************************************/

#include "delaunay.h"

interface(argc,argv) 

    int	     argc;			/* Number of command line arguments.		*/
    char    *argv[];			/* Contents of command line arguments.		*/

{
    /*----------------------------------------------------------------------------------*/
    /*                                 INITIALISE					*/
    /*----------------------------------------------------------------------------------*/ 

    struct Option 	*in_sites;	/* This is a pointer to a structure holding	*/
					/* the all the text to describe each option.	*/
					/* It also stores the user's input.		*/

    struct Option	*out_vect;	/* Identical to *in_sites; same struct, diff-	*/
					/* erent contents. Further options possible.	*/

    struct Option	*scaling;	/* Vertical scaling of elevation values.	*/

    struct Flag		*vrml_out;	/* Flag to set VRML1.0 output.			*/

    G_gisinit (argv[0]);                /* This GRASS library function MUST be called   */
                                      	/* first to check for valid database and mapset.*/
                              		/* As usual argv[0] is the program name. This   */
                               		/* can be recalled using G_program_name().      */

    /*----------------------------------------------------------------------------------*/
    /*                              SET PARSER OPTIONS 					*/
    /*----------------------------------------------------------------------------------*/

    in_sites = G_define_option();	/* Request pointer to memory for each option	*/
    out_vect = G_define_option();	
    scaling  = G_define_option();
    vrml_out = G_define_flag();

    /* Each option needs a 'key' (short description), a 'description` (a longer one)	*/
    /* a 'type' (eg intiger, or string), and an indication whether manditory or not	*/

    in_sites->key	  = "in";
    in_sites->description = "Sites file to process";
    in_sites->type	  = TYPE_STRING;
    in_sites->required	  = YES;


    out_vect->key	  = "out";
    out_vect->description = "Vector map layer to contain delaunay triangulation.";
    out_vect->type	  = TYPE_STRING;
    out_vect->required	  = YES;

    scaling->key	  = "zscale";
    scaling->description = "Vertical scaling of elevation.";
    scaling->type	  = TYPE_DOUBLE;
    scaling->required	  = NO;
    scaling->answer	  = "1.0";

    vrml_out->key	  = 'v';
    vrml_out->description = "Create VRML 1.0 output";
    

    if (G_parser(argc,argv))		/* Actually performs the prompting for 		*/
	exit(-1);			/* keyboard input. Returns a 0 if sucessful.	*/

    sites_in_name = in_sites->answer;	/* Now that keyboard input has been parsed, we	*/
    vect_out_name = out_vect->answer;	/* can place the contents into our string arrays*/
    sscanf(scaling->answer,"%f",&zscale);
    vrml          = vrml_out->answer;	


    /*----------------------------------------------------------------------------------*/
    /*                           CHECK INPUT SITES FILE EXISTS				*/
    /*----------------------------------------------------------------------------------*/
 
 
    if ((mapset_in=G_find_file("site_lists",sites_in_name,"")) == NULL)
    {
        char err[256];
        sprintf(err,"Cannot find sites file [%s].",sites_in_name);
        G_fatal_error(err);
    }

    /*----------------------------------------------------------------------------------*/
    /*                       CHECK OUTPUT VECTOR FILE DOES NOT EXIST			*/
    /*----------------------------------------------------------------------------------*/

    mapset_out = G_mapset();		/* Current mapset.				*/

    if (G_legal_filename(vect_out_name)==NULL)
    {
        char err[256];
        sprintf(err,"Illegal file name. Please try another.");
        G_fatal_error(err);
    }
    else
    {
        if (G_find_vector2(vect_out_name,mapset_out) !=NULL)
        {
            char err[256];
            sprintf(err,"Vector map [%s] exists.\nPlease try another\n",vect_out_name);
            G_fatal_error(err);
        }
    }
}
