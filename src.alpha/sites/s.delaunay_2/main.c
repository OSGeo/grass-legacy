/***********************************************************************************/
/***                                                                             ***/
/***                               s.delaunay                                    ***/
/*** GRASS module to calculate the delaunay triangulation from sites files.      ***/
/***  										 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, 19th July, 1995                                   ***/
/***                                                                             ***/
/***********************************************************************************/

#define MAIN			/* Must come before including maproj.h		*/

#include "delaunay.h"		/* Must be included for program to work		*/

main(argc,argv) 
    int argc;
    char *argv[];		/* Assuming GRASS is going to respond to some	*/
				/* input from the keyboard, the two arguments	*/
				/* are necessary. argc is an ARGument Count 	*/
				/* that stores the number of words input. *argv	*/
				/* is a POINTER to an array holding those words.*/
{

    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE					*/
    /*--------------------------------------------------------------------------*/ 

    struct Map_info Map_out;	/* Vector information for output file.		*/



    /*--------------------------------------------------------------------------*/
    /*                          GET INPUT FROM USER				*/
    /*--------------------------------------------------------------------------*/

    interface(argc,argv);


    /*--------------------------------------------------------------------------*/
    /*                    OPEN INPUT AND OUTPUT VECTOR FILES			*/
    /*--------------------------------------------------------------------------*/

    open_files(&Map_out);


    /*--------------------------------------------------------------------------*/
    /*                       PROCESS VECTOR COORDINATES				*/
    /*--------------------------------------------------------------------------*/
   

    process(&Map_out);


    /*--------------------------------------------------------------------------*/
    /*                 CLOSE ALL OPENED FILES AND FREE MEMORY			*/
    /*--------------------------------------------------------------------------*/

    Vect_close(&Map_out);
    fclose(sites_fptr);

}
