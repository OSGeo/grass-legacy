/***********************************************************************************/
/***                                                                             ***/
/***                                 m.in.ntf                                	 ***/
/***		 GRASS module to read in any Ordnance Survey NTF data. 		 ***/
/***  		   Assumes National Transfer Format V2.0 (BS 7567).		 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.1 19th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#define MAIN

#include "ntf_in.h"			/* Must be included for program to work		*/

main(argc,argv) 
    int argc;
    char *argv[];		
{

    /*----------------------------------------------------------------------------------*/
    /*                           INITIALISE GLOBAL VARIABLES				*/
    /*----------------------------------------------------------------------------------*/ 

    O_raster	= FALSE;
    O_vector	= FALSE;

    O_cont	= FALSE;
O_temp	= FALSE;
    O_spot	= FALSE;
    O_lake	= FALSE;
    O_break	= FALSE;
    O_coast	= FALSE;
    O_form	= FALSE;

    num_rlines	= 0;

    X_min 	= 0;
    Y_min	= 0;
    X_max	= 0;
    Y_max	= 0;
    X_origin	= 0;
    Y_origin	= 0;

    XY_mult	= 1.0;
    Z_mult	= 1.0;

    strcpy (H_organ,"Not Known                    ");
    strcpy (H_ddate,"          ");
    strcpy (H_mname,"Not Known                              ");
    strcpy (H_mdate,"          ");
    strcpy (V_name,"                                        ");
    H_scale = 0;

    points = Vect_new_line_struct();

    Write_vect = FALSE;

				/* Initialise the two category classess, one for vector	*/
				/* classes, the other for feature descriptions.		*/
    G_init_cats((CELL)0,"Ordnance Survey NTF",&cats);
    G_init_cats((CELL)0,"Feature Descriptions",&feature_desc);


    /*----------------------------------------------------------------------------------*/
    /*                               GET INPUT FROM USER				*/
    /*----------------------------------------------------------------------------------*/

    interface(argc,argv);


    /*----------------------------------------------------------------------------------*/
    /*                          OPEN INPUT AND OUTPUT FILES			*/
    /*----------------------------------------------------------------------------------*/

    open_files();


    /*----------------------------------------------------------------------------------*/
    /*                                PROCESS NTF FILE					*/
    /*----------------------------------------------------------------------------------*/
   
    read_ntf();


    /*----------------------------------------------------------------------------------*/
    /*                     CLOSE ALL OPENED FILES AND FREE MEMORY			*/
    /*----------------------------------------------------------------------------------*/

    if (outfile)
    	close_vect();

    fclose(ntf_fptr);

    G_free_cats(&cats);
    G_free_cats(&feature_desc);


}
