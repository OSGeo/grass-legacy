/*********************************************************************************/
/**	                                                            		**/
/**                              r.param.scale                 			**/
/**       GRASS module for extracting multi-scale surface parameters.		**/
/**  										**/
/**                                                         			**/
/**			  Jo Wood, V 1.1, 11th December, 1994			**/
/**                                      					**/
/*********************************************************************************/

#define MAIN

#include "param.h"	

main(argc,argv) 
    int argc;
    char *argv[];
{

    /*--------------------------------------------------------------------------*/
    /*                                 INITIALISE				*/
    /*--------------------------------------------------------------------------*/ 



    /*--------------------------------------------------------------------------*/
    /*                               GET INPUT FROM USER			*/
    /*--------------------------------------------------------------------------*/

    interface(argc,argv);


    /*--------------------------------------------------------------------------*/
    /*                        OPEN INPUT AND OUTPUT RASTER FILES		*/
    /*--------------------------------------------------------------------------*/

    open_files();
    init_graphics();


    /*--------------------------------------------------------------------------*/
    /*                       PROCESS SURFACE FOR FEATURE DETECTION 		*/
    /*--------------------------------------------------------------------------*/

    process();   

    /*--------------------------------------------------------------------------*/
    /*                     CLOSE ALL OPENED FILES AND FREE MEMORY		*/
    /*--------------------------------------------------------------------------*/

    close_down();

}