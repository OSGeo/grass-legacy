/**********************************************************************************/
/***                                                                           	***/
/***                                  r.sfeature                               	***/
/*** GRASS module for surface feature thinning  (pits, peaks, saddles only).   	***/
/***  										***/
/***                                                                            ***/
/***  Jo Wood, department of Geography, July, 1995.                             ***/
/***                                                                            ***/
/**********************************************************************************/

#define MAIN			/* Must come before including maproj.h		*/

#include "feature.h"		/* Must be included for program to work		*/

main(argc,argv) 
    int argc;
    char *argv[];		/* Assuming GRASS is going to respond to some	*/
				/* input from the keyboard, the two arguments	*/
				/* are necessary. argc is an ARGument Count 	*/
				/* that stores the number of words input. *argv	*/
				/* is a POINTER to an array holding those words.*/
{

    /*---------------------------------------------------------------------------*/
    /*                               INITIALISE					 */
    /*---------------------------------------------------------------------------*/ 



    /*---------------------------------------------------------------------------*/
    /*                               GET INPUT FROM USER		 	 */
    /*---------------------------------------------------------------------------*/

    interface(argc,argv);


    /*---------------------------------------------------------------------------*/
    /*                        OPEN INPUT AND OUTPUT RASTER FILES		 */
    /*---------------------------------------------------------------------------*/

    open_files();


    /*---------------------------------------------------------------------------*/
    /*                       PROCESS SURFACE FOR FEATURE DETECTION 		 */
    /*---------------------------------------------------------------------------*/

    process();   

    /*---------------------------------------------------------------------------*/
    /*                     CLOSE ALL OPENED FILES AND FREES MEMORY		 */
    /*---------------------------------------------------------------------------*/

    close_down();

    /*---------------------------------------------------------------------------*/
    /*                    CREATE SUPPORT FILES (COLOUR, CATEGORIES ETC)		 */
    /*---------------------------------------------------------------------------*/

    if(rast_out_name)
    {
    	write_cats();
    	write_cols();
    }

}
